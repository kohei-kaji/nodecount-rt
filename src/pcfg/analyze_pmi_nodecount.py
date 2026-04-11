import argparse
import csv
import glob
import math
import os
import sys
import time
from collections import defaultdict

import numpy as np
import torch
from nltk.tree import Tree
from scipy import stats as scipy_stats

sys.path.insert(0, os.path.dirname(__file__))
from ud2bin import parse_conllu_sentence, is_projective, build_strict_binary_tree
from pcfg import count_word_freq, replace_rare_words_in_tree, extract_pcfg, compute_unigram, PCFGAnalyzer, prepare_test_words


def load_conllu_sentences(folder):
    conllu_files = sorted(glob.glob(os.path.join(folder, "*.conllu")))
    if not conllu_files:
        sys.exit(f"Error: No .conllu files found in '{folder}'")

    sentences, trees = [], []
    n_nonproj = n_noroot = 0

    for file_path in conllu_files:
        with open(file_path, encoding="utf-8") as f:
            raw_lines = []
            for line in f:
                if line.strip() == "":
                    if raw_lines:
                        result = _parse_sentence(raw_lines)
                        if result is None:
                            n_noroot += 1
                        elif result == "nonproj":
                            n_nonproj += 1
                        else:
                            sentences.append(result[0])
                            trees.append(result[1])
                        raw_lines = []
                else:
                    raw_lines.append(line)
            if raw_lines:
                result = _parse_sentence(raw_lines)
                if result is None:
                    n_noroot += 1
                elif result == "nonproj":
                    n_nonproj += 1
                else:
                    sentences.append(result[0])
                    trees.append(result[1])

    print(f"Loaded {len(sentences)} projective sentences "
          f"(skipped: {n_nonproj} non-projective, {n_noroot} no-root)")
    return sentences, trees


def _parse_sentence(raw_lines):
    nodes, children_map = parse_conllu_sentence(raw_lines)
    root_ids = children_map.get(0, [])
    if not nodes or not root_ids:
        return None
    if not is_projective(nodes):
        return "nonproj"

    tokens = []
    for line in raw_lines:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        parts = line.split('\t')
        if not parts[0].isdigit():
            continue
        tokens.append({
            "id":     int(parts[0]),
            "form":   parts[1],
            "pos":    parts[3],
            "head":   int(parts[6]),
            "deprel": parts[7],
        })

    tree = Tree("ROOT", [build_strict_binary_tree(root_ids[0], nodes, children_map)])
    return tokens, tree


EXCLUDED_DEPRELS = {"punct", "root", "dep", "reparandum"}


def compute_nodecount(tokens, exclude_punct=True):
    active = [t for t in tokens if t["deprel"] not in EXCLUDED_DEPRELS] if exclude_punct else tokens
    active_ids = {t["id"] for t in active}

    nc_active = []
    for t in active:
        count = sum(1 for a in active if a["id"] < t["id"] and a["head"] == t["id"])
        if t["head"] != 0 and t["head"] < t["id"] and t["head"] in active_ids:
            count += 1
        nc_active.append(count)

    nc_iter = iter(nc_active)
    return [next(nc_iter) if (t["deprel"] not in EXCLUDED_DEPRELS or not exclude_punct) else None for t in tokens]


@torch.no_grad()
def compute_pmi_interpolated(words, analyser_lex, analyser_van, lam, unigram_probs):
    """
    P_interp(w_0..w_k) = λ·P_lex(w_0..w_k) + (1-λ)·P_van(w_0..w_k)
    """
    inside_lex = analyser_lex.compute_inside(words)
    prefix_lex = analyser_lex.compute_prefix_table(words, inside_lex)
    inside_van = analyser_van.compute_inside(words)
    prefix_van = analyser_van.compute_prefix_table(words, inside_van)

    metrics = []
    for k in range(len(words)):
        pp_lex_k    = prefix_lex[(0, k)][analyser_lex.S_idx].item()
        pp_lex_prev = prefix_lex[(0, k-1)][analyser_lex.S_idx].item() if k > 0 else 1.0
        pp_van_k    = prefix_van[(0, k)][analyser_van.S_idx].item()
        pp_van_prev = prefix_van[(0, k-1)][analyser_van.S_idx].item() if k > 0 else 1.0

        pp_k    = lam * pp_lex_k    + (1.0 - lam) * pp_van_k
        pp_prev = lam * pp_lex_prev + (1.0 - lam) * pp_van_prev
        p_c = (pp_k / pp_prev) if pp_prev > 0 else 0.0
        p_u = unigram_probs.get(words[k], 1e-10)

        surp_u = -math.log2(p_u) if p_u > 0 else float('inf')
        surp_c = -math.log2(p_c) if p_c > 0 else float('inf')
        metrics.append({"word": words[k], "P_u": p_u, "P_c": p_c, "surp_u": surp_u, "surp_c": surp_c, "pmi": surp_u - surp_c})
    return metrics


def run_bootstrap(train_trees_proc, test_trees, vocab, unigram, n_bootstrap, lexicalize, lam, device):
    n_train = len(train_trees_proc)
    rng = np.random.default_rng(42)
    use_interp = lexicalize and lam < 1.0
    pmi_boot = defaultdict(list)

    for b in range(n_bootstrap):
        idxs = rng.integers(0, n_train, size=n_train)
        boot_trees = [train_trees_proc[i] for i in idxs]

        try:
            rules_b, start_b, term_b = extract_pcfg(boot_trees, lexicalize=lexicalize)
            ana_b = PCFGAnalyzer(rules_b, start_b, term_b, device=device)
            ana_van_b = None
            if use_interp:
                rules_van_b, _, term_van_b = extract_pcfg(boot_trees, lexicalize=False)
                ana_van_b = PCFGAnalyzer(rules_van_b, start_b, term_van_b, device=device)
        except Exception as e:
            print(f"  [Bootstrap {b+1}] build failed: {e}", file=sys.stderr)
            continue

        for sent_idx, tree in enumerate(test_trees):
            words = prepare_test_words(tree, vocab)
            try:
                if use_interp:
                    metrics = compute_pmi_interpolated(words, ana_b, ana_van_b, lam, unigram)
                else:
                    metrics = ana_b.compute_pmi(words, unigram)
                for pos, m in enumerate(metrics):
                    pmi_boot[(sent_idx, pos)].append(m["pmi"])
            except Exception:
                pass

        print(f"  Bootstrap {b+1}/{n_bootstrap} done")

    return pmi_boot


def _weighted_pearsonr(x, y, w):
    w = w / w.sum()
    mx = np.average(x, weights=w)
    my = np.average(y, weights=w)
    cov = np.sum(w * (x - mx) * (y - my))
    sx  = np.sqrt(np.sum(w * (x - mx) ** 2))
    sy  = np.sqrt(np.sum(w * (y - my) ** 2))
    return cov / (sx * sy) if sx * sy > 0 else float('nan')


def compute_correlations(all_rows):
    valid = [r for r in all_rows if r["nodecount"] is not None and math.isfinite(r["pmi"]) and math.isfinite(r["surp_c"])]
    n_total = sum(1 for r in all_rows if r["nodecount"] is not None)
    n_inf   = n_total - len(valid)

    nc  = np.array([r["nodecount"] for r in valid])
    pmi = np.array([r["pmi"]       for r in valid])
    su  = np.array([r["surp_u"]    for r in valid])
    sc  = np.array([r["surp_c"]    for r in valid])

    for label, y in [("PMI", pmi), ("Surprisal_u", su), ("Surprisal_c", sc)]:
        r_p, p_p = scipy_stats.pearsonr(nc, y)
        r_s, p_s = scipy_stats.spearmanr(nc, y)
        print(f"  Nodecount ~ {label:12s}:  "
              f"Pearson r={r_p:+.4f} (p={p_p:.3e}),  "
              f"Spearman ρ={r_s:+.4f} (p={p_s:.3e})")

    if "pmi_sd" in valid[0]:
        sd_vals = np.array([r["pmi_sd"] for r in valid])
        finite_mask = np.isfinite(sd_vals)
        print(f"\n=== bootstrap SD ===")
        print(f"  valid samples: {finite_mask.sum()}")
        print(f"  mean SD: {sd_vals[finite_mask].mean():.4f},  "
              f"median: {np.median(sd_vals[finite_mask]):.4f},  "
              f"90%: {np.percentile(sd_vals[finite_mask], 90):.4f}")

        stable = [r for r in valid if math.isfinite(r["pmi_sd"]) and r["pmi_sd"] > 0]
        if stable:
            w   = 1.0 / np.array([r["pmi_sd"] ** 2 for r in stable])
            nc_s  = np.array([r["nodecount"] for r in stable])
            pmi_s = np.array([r["pmi"]       for r in stable])
            r_w = _weighted_pearsonr(nc_s, pmi_s, w)
            r_uw, p_uw = scipy_stats.pearsonr(nc_s, pmi_s)
            print(f"  Pearson: r={r_uw:+.4f}")
            print(f"  Weighted Pearson: r={r_w:+.4f}")

        nc_groups = defaultdict(list)
        for r in valid:
            nc_groups[int(r["nodecount"])].append(r)
        for k in sorted(nc_groups):
            rs = nc_groups[k]
            pmi_arr = np.array([r["pmi"] for r in rs])
            sd_arr  = np.array([r["pmi_sd"] for r in rs if math.isfinite(r["pmi_sd"])])
            sd_str  = f"mean_SD={sd_arr.mean():.3f}" if len(sd_arr) > 0 else "mean_SD=n/a"
            print(f"  nc={k:2d}: n={len(rs):5d}  "
                  f"mean_pmi={pmi_arr.mean():+.4f}  sd_pmi={pmi_arr.std():.4f}  {sd_str}")
    else:
        nc_groups = defaultdict(list)
        for r in valid:
            nc_groups[int(r["nodecount"])].append(r["pmi"])
        for k in sorted(nc_groups):
            vals = nc_groups[k]
            print(f"  nc={k:2d}: n={len(vals):5d}  "
                  f"mean_pmi={np.mean(vals):+.4f}  sd={np.std(vals):.4f}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--data", default="../../data/UD_English-GUM")
    parser.add_argument("--train-ratio", type=float, default=0.9)
    parser.add_argument("--min-freq", type=int, default=2)
    parser.add_argument("--lexicalize", action="store_true")
    parser.add_argument("--lam", type=float, default=1.0, help="λ: 1.0=fully lexicalized, 0.0=vanilla")
    parser.add_argument("--max-sent-len", type=int, default=20)
    parser.add_argument("--n-bootstrap", type=int, default=0, help="the number of bootstrap samples (0 to skip)")
    parser.add_argument("--output-csv", default="../../result/pmi_nodecount.csv")
    parser.add_argument("--device", default=None)
    parser.add_argument("--no-exclude-punct", dest="exclude_punct", action="store_false", default=True)
    args = parser.parse_args()

    device = args.device
    use_interp = args.lexicalize and args.lam < 1.0
    print(f"Device: {device}")

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
    rules, start_sym, terminals = extract_pcfg(
        train_trees_proc, lexicalize=args.lexicalize)
    label = "Lexicalized" if args.lexicalize else "Vanilla"
    print(f"PCFG ({label}): {len(rules)} NTs, "
          f"{sum(len(v) for v in rules.values())} rules  ({time.time()-t0:.2f}s)")

    t0 = time.time()
    analyser = PCFGAnalyzer(rules, start_sym, terminals, device=device)
    print(f"Analyser built ({analyser.M} NTs, {time.time()-t0:.2f}s)")

    analyser_van = None
    if use_interp:
        t0 = time.time()
        rules_van, _, terminals_van = extract_pcfg(train_trees_proc, lexicalize=False)
        analyser_van = PCFGAnalyzer(rules_van, start_sym, terminals_van, device=device)
        print(f"Vanilla analyser built ({analyser_van.M} NTs, {time.time()-t0:.2f}s)"
              f"  [λ={args.lam}]")

    all_rows = []
    skipped = 0
    for sent_idx, (tokens, tree) in enumerate(zip(test_sents, test_trees)):
        words = prepare_test_words(tree, vocab)
        try:
            if use_interp:
                pmi_metrics = compute_pmi_interpolated(words, analyser, analyser_van, args.lam, unigram)
            else:
                pmi_metrics = analyser.compute_pmi(words, unigram)
        except Exception as e:
            print(f"  [WARN] sent {sent_idx}: {e}", file=sys.stderr)
            skipped += 1
            continue

        if len(tokens) != len(pmi_metrics):
            skipped += 1
            continue

        nc_values = compute_nodecount(tokens, exclude_punct=args.exclude_punct)
        for tok, pm, nc in zip(tokens, pmi_metrics, nc_values):
            all_rows.append({
                "sent_id":   sent_idx,
                "token_id":  tok["id"],
                "form":      tok["form"],
                "pos":       tok["pos"],
                "deprel":    tok["deprel"],
                "pmi":       pm["pmi"],
                "surp_u":    pm["surp_u"],
                "surp_c":    pm["surp_c"],
                "nodecount": nc,
            })

    print(f"\nTotal words: {len(all_rows)}  (skipped {skipped} sentences)")

    if args.n_bootstrap > 0:
        print(f"\nRunning bootstrap (B={args.n_bootstrap}) ...")
        pmi_boot = run_bootstrap(
            train_trees_proc, test_trees, vocab, unigram,
            args.n_bootstrap, args.lexicalize, args.lam, device)

        for row in all_rows:
            key  = (row["sent_id"], row["token_id"] - 1)
            vals = [v for v in pmi_boot.get(key, []) if math.isfinite(v)]
            row["pmi_sd"] = float(np.std(vals)) if len(vals) >= 2 else float("nan")
            row["pmi_zero_rate"] = sum(1 for v in pmi_boot.get(key, []) if not math.isfinite(v)) / max(1, len(pmi_boot.get(key, [])))

    compute_correlations(all_rows)

    out_path = os.path.normpath(os.path.join(base_dir, args.output_csv))
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    fieldnames = ["sent_id", "token_id", "form", "pos", "deprel", "pmi", "surp_u", "surp_c", "nodecount"]
    if args.n_bootstrap > 0:
        fieldnames += ["pmi_sd", "pmi_zero_rate"]
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(all_rows)
    print(f"Saved: {out_path}")


if __name__ == "__main__":
    main()
