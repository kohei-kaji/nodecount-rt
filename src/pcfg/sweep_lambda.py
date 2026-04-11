import argparse
import csv
import math
import os
import sys
import time

import numpy as np
import torch
from scipy import stats as scipy_stats

sys.path.insert(0, os.path.dirname(__file__))
from pcfg import count_word_freq, replace_rare_words_in_tree, extract_pcfg, compute_unigram, PCFGAnalyzer, prepare_test_words
from analyze_pmi_nodecount import load_conllu_sentences, compute_nodecount, _weighted_pearsonr, EXCLUDED_DEPRELS


@torch.no_grad()
def collect_prefix_probs(words, analyser_lex, analyser_van, unigram_probs):
    inside_lex = analyser_lex.compute_inside(words)
    prefix_lex = analyser_lex.compute_prefix_table(words, inside_lex)
    inside_van = analyser_van.compute_inside(words)
    prefix_van = analyser_van.compute_prefix_table(words, inside_van)

    result = []
    for k in range(len(words)):
        result.append({
            "pp_lex_k":    prefix_lex[(0, k)][analyser_lex.S_idx].item(),
            "pp_lex_prev": prefix_lex[(0, k-1)][analyser_lex.S_idx].item() if k > 0 else 1.0,
            "pp_van_k":    prefix_van[(0, k)][analyser_van.S_idx].item(),
            "pp_van_prev": prefix_van[(0, k-1)][analyser_van.S_idx].item() if k > 0 else 1.0,
            "p_u":         unigram_probs.get(words[k], 1e-10),
        })
    return result


def pmi_from_prefix(pp_lex_k, pp_lex_prev, pp_van_k, pp_van_prev, p_u, lam):
    pp_k    = lam * pp_lex_k    + (1.0 - lam) * pp_van_k
    pp_prev = lam * pp_lex_prev + (1.0 - lam) * pp_van_prev
    p_c = pp_k / pp_prev if pp_prev > 0 else 0.0
    surp_u = -math.log2(p_u) if p_u > 0 else float('inf')
    surp_c = -math.log2(p_c) if p_c > 0 else float('inf')
    return surp_u - surp_c



def run_bootstrap(train_trees_proc, test_trees, vocab, unigram, n_bootstrap, device):
    n_train = len(train_trees_proc)
    rng = np.random.default_rng(42)
    boot_probs = []

    for b in range(n_bootstrap):
        idxs = rng.integers(0, n_train, size=n_train)
        boot_trees = [train_trees_proc[i] for i in idxs]

        try:
            rules_lex, start_b, term_lex = extract_pcfg(boot_trees, lexicalize=True)
            ana_lex = PCFGAnalyzer(rules_lex, start_b, term_lex, device=device)
            rules_van, _, term_van = extract_pcfg(boot_trees, lexicalize=False)
            ana_van = PCFGAnalyzer(rules_van, start_b, term_van, device=device)
        except Exception as e:
            print(f"  [Bootstrap {b+1}] build failed: {e}", file=sys.stderr)
            boot_probs.append(None)
            continue

        sent_probs = []
        for tree in test_trees:
            words = prepare_test_words(tree, vocab)
            try:
                probs = collect_prefix_probs(words, ana_lex, ana_van, unigram)
            except Exception:
                probs = [None] * len(words)
            sent_probs.append(probs)

        boot_probs.append(sent_probs)
        print(f"  Bootstrap {b+1}/{n_bootstrap} done")

    return boot_probs



def compute_correlations_for_lambda(word_entries, lam, n_bootstrap):
    pmis, ncs, pmi_sds = [], [], []

    for e in word_entries:
        if e["nodecount"] is None:
            continue

        pmi = pmi_from_prefix(e["pp_lex_k"], e["pp_lex_prev"], e["pp_van_k"], e["pp_van_prev"], e["p_u"], lam)
        if not math.isfinite(pmi):
            continue

        pmis.append(pmi)
        ncs.append(e["nodecount"])

        if n_bootstrap > 0:
            boot_vals = []
            for bp in e["boot_pp"]:
                if bp is None:
                    continue
                bpmi = pmi_from_prefix(bp["pp_lex_k"], bp["pp_lex_prev"], bp["pp_van_k"], bp["pp_van_prev"], e["p_u"], lam)
                if math.isfinite(bpmi):
                    boot_vals.append(bpmi)
            pmi_sds.append(np.std(boot_vals) if len(boot_vals) >= 2 else float("nan"))

    if len(pmis) < 10:
        return None

    pmis = np.array(pmis)
    ncs  = np.array(ncs)

    r_p, p_p = scipy_stats.pearsonr(ncs, pmis)
    r_s, p_s = scipy_stats.spearmanr(ncs, pmis)

    result = {
        "lam": lam,
        "n_valid": len(pmis),
        "pearson_r": r_p,
        "pearson_p": p_p,
        "spearman_r": r_s,
        "spearman_p": p_s,
        "mean_pmi": float(pmis.mean()),
        "sd_pmi": float(pmis.std()),
        "weighted_pearson_r": float("nan"),
        "mean_boot_sd": float("nan"),
    }

    if n_bootstrap > 0 and pmi_sds:
        sd_arr = np.array(pmi_sds)
        finite = np.isfinite(sd_arr)
        if finite.sum() > 0:
            result["mean_boot_sd"] = float(sd_arr[finite].mean())

        pos = finite & (sd_arr > 0)
        if pos.sum() >= 10:
            w   = 1.0 / sd_arr[pos] ** 2
            result["weighted_pearson_r"] = _weighted_pearsonr(ncs[pos], pmis[pos], w)

    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data", default="../../data/UD_English-GUM")
    parser.add_argument("--train-ratio", type=float)
    parser.add_argument("--min-freq", type=int, default=5)
    parser.add_argument("--max-sent-len", type=int, default=40)
    parser.add_argument("--lambdas", type=float, nargs="+", default=[round(x, 2) for x in np.linspace(0, 1, 11)])
    parser.add_argument("--n-bootstrap", type=int, default=50)
    parser.add_argument("--output-csv", default="../../result/sweep_lambda.csv")
    parser.add_argument("--device", default=None)
    parser.add_argument("--no-exclude-punct", dest="exclude_punct", action="store_false", default=True)
    args = parser.parse_args()

    device = args.device
    print(f"Device: {device}")
    print(f"Lambda values: {args.lambdas}")

    base_dir = os.path.dirname(__file__)
    data_dir = os.path.normpath(os.path.join(base_dir, args.data))

    sentences, trees = load_conllu_sentences(data_dir)
    filtered = [(s, t) for s, t in zip(sentences, trees) if len(s) <= args.max_sent_len]
    print(f"After length filter (<= {args.max_sent_len}): {len(filtered)} sentences")
    sentences = [x[0] for x in filtered]
    trees     = [x[1] for x in filtered]

    n_train = int(len(trees) * args.train_ratio)
    train_trees = trees[:n_train]
    test_trees  = trees[n_train:]
    test_sents  = sentences[n_train:]
    print(f"Train: {len(train_trees)}  Test: {len(test_trees)}")

    freq  = count_word_freq(train_trees)
    vocab = {w for w, c in freq.items() if c >= args.min_freq}
    print(f"Vocab size (freq >= {args.min_freq}): {len(vocab)}")

    train_trees_proc = [replace_rare_words_in_tree(t, vocab) for t in train_trees]
    unigram = compute_unigram(train_trees_proc)

    t0 = time.time()
    rules_lex, start_sym, term_lex = extract_pcfg(train_trees_proc, lexicalize=True)
    analyser_lex = PCFGAnalyzer(rules_lex, start_sym, term_lex, device=device)
    print(f"Lex analyser built ({analyser_lex.M} NTs, {time.time()-t0:.2f}s)")

    t0 = time.time()
    rules_van, _, term_van = extract_pcfg(train_trees_proc, lexicalize=False)
    analyser_van = PCFGAnalyzer(rules_van, start_sym, term_van, device=device)
    print(f"Van analyser built ({analyser_van.M} NTs, {time.time()-t0:.2f}s)")

    print("\nCollecting prefix probs from test sentences ...")
    word_entries = []
    skipped = 0

    for sent_idx, (tokens, tree) in enumerate(zip(test_sents, test_trees)):
        words = prepare_test_words(tree, vocab)
        if len(tokens) != len(words):
            skipped += 1
            continue

        try:
            probs = collect_prefix_probs(words, analyser_lex, analyser_van, unigram)
        except Exception as e:
            print(f"  [WARN] sent {sent_idx}: {e}", file=sys.stderr)
            skipped += 1
            continue

        nc_values = compute_nodecount(tokens, exclude_punct=args.exclude_punct)

        for tok, pr, nc in zip(tokens, probs, nc_values):
            word_entries.append({
                "sent_id": sent_idx,
                "token_id": tok["id"],
                "form": tok["form"],
                "pos": tok["pos"],
                "deprel": tok["deprel"],
                "nodecount": nc,
                "pp_lex_k": pr["pp_lex_k"],
                "pp_lex_prev": pr["pp_lex_prev"],
                "pp_van_k": pr["pp_van_k"],
                "pp_van_prev": pr["pp_van_prev"],
                "p_u": pr["p_u"],
                "boot_pp": [],
            })

    print(f"Total words: {len(word_entries)}  (skipped {skipped} sentences)")

    if args.n_bootstrap > 0:
        print(f"\nRunning bootstrap (B={args.n_bootstrap}) ...")
        boot_probs = run_bootstrap(train_trees_proc, test_trees, vocab, unigram, args.n_bootstrap, device)

        entry_idx = 0
        for sent_idx, (tokens, tree) in enumerate(zip(test_sents, test_trees)):
            words = prepare_test_words(tree, vocab)
            if len(tokens) != len(words):
                continue
            for pos in range(len(tokens)):
                boot_pp_for_word = []
                for b, sent_probs in enumerate(boot_probs):
                    if sent_probs is None:
                        boot_pp_for_word.append(None)
                        continue
                    word_prob = sent_probs[sent_idx][pos] if pos < len(sent_probs[sent_idx]) else None
                    boot_pp_for_word.append(word_prob)
                word_entries[entry_idx]["boot_pp"] = boot_pp_for_word
                entry_idx += 1

    print(f"{'λ':>6}  {'n':>6}  {'Pearson':>9}  {'Spearman':>9} {'Wt.Pearson':>10}  {'mean_SD':>8}")

    results = []
    for lam in args.lambdas:
        res = compute_correlations_for_lambda(word_entries, lam, args.n_bootstrap)
        if res is None:
            continue
        results.append(res)

        wt = f"{res['weighted_pearson_r']:+.4f}" if math.isfinite(res['weighted_pearson_r']) else "   n/a"
        sd = f"{res['mean_boot_sd']:.4f}"         if math.isfinite(res['mean_boot_sd'])       else "   n/a"
        print(f"  {lam:.2f}  {res['n_valid']:>6}  "
              f"{res['pearson_r']:+.4f} ({res['pearson_p']:.2e})  "
              f"{res['spearman_r']:+.4f} ({res['spearman_p']:.2e})  "
              f"{wt:>10}  {sd:>8}")

    out_path = os.path.normpath(os.path.join(base_dir, args.output_csv))
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    fieldnames = ["lam", "n_valid", "pearson_r", "pearson_p", "spearman_r", "spearman_p", "weighted_pearson_r", "mean_boot_sd", "mean_pmi", "sd_pmi"]
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(results)


if __name__ == "__main__":
    main()
