"""
Exact PMI computation from PCFGs using left-corner matrix inversion.

- Handles left-recursion via R_L = (I - P_L)^{-1}
- Uses PyTorch for GPU-accelerated matrix ops and vectorised CKY
- Replaces rare words (below frequency threshold) with their POS tag
"""

import math
import os
import sys
import time
import argparse
from collections import defaultdict
from functools import lru_cache

import torch
import numpy as np
from nltk.tree import Tree


# ============================================================
# 1. Tree pre-processing
# ============================================================

def count_word_freq(trees):
    """Count word frequencies across all trees."""
    freq = defaultdict(int)
    for tree in trees:
        for word in tree.leaves():
            freq[word.lower()] += 1
    return freq


def replace_rare_words_in_tree(tree, vocab, copy=True):
    """Replace words not in *vocab* with their pre-terminal (POS) label.

    Works on Penn Treebank-style trees where the parent of each leaf
    is a POS tag node.
    """
    t = tree.copy(deep=True) if copy else tree
    for pos in t.treepositions('leaves'):
        word = t[pos].lower()
        if word not in vocab:
            parent_pos = pos[:-1]
            pos_tag = t[parent_pos].label()
            t[pos] = pos_tag.lower()       # use lowercased POS as surface form
    return t


def percolate_heads(tree, root_label="ROOT"):
    """Percolate lexical heads upward for lexicalised PCFG.

    The *root_label* node is left unlexicalised so that start symbol
    stays the same across all sentences (e.g. ROOT → VERB[saw]).
    """
    if isinstance(tree, str):
        return tree
    if len(tree) == 1 and isinstance(tree[0], str):
        head = tree[0].lower()
        tree.set_label(f"{tree.label()}[{head}]")
        return head
    child_heads = [percolate_heads(c, root_label) for c in tree]
    base = tree.label().split('[')[0]
    head = child_heads[0]
    for i, child in enumerate(tree):
        if not isinstance(child, str):
            if child.label().split('[')[0] == base:
                head = child_heads[i]
                break
    # Do NOT lexicalise the root node
    if base != root_label:
        tree.set_label(f"{base}[{head}]")
    return head


# ============================================================
# 2. PCFG extraction
# ============================================================

def extract_pcfg(trees, lexicalize=False):
    """Extract a (lexicalised / unlexicalised) PCFG from *trees*.

    Returns
    -------
    rules : dict[str, list[tuple[tuple[str,...], float]]]
    start_symbol : str
    terminals : set[str]
    """
    rule_counts = defaultdict(lambda: defaultdict(int))
    lhs_counts  = defaultdict(int)
    terminals   = set()

    base_root = trees[0].label()          # e.g. "ROOT" or "S"

    for tree in trees:
        t = tree.copy(deep=True)
        if lexicalize:
            percolate_heads(t, root_label=base_root)
        for prod in t.productions():
            lhs = prod.lhs().symbol()
            rhs = tuple(
                child if isinstance(child, str) else child.symbol()
                for child in prod.rhs()
            )
            rule_counts[lhs][rhs] += 1
            lhs_counts[lhs] += 1
            if prod.is_lexical():
                terminals.add(rhs[0].lower())

    rules = defaultdict(list)
    for lhs, rhs_dict in rule_counts.items():
        for rhs, cnt in rhs_dict.items():
            rules[lhs].append((rhs, cnt / lhs_counts[lhs]))

    start_sym = base_root

    return dict(rules), start_sym, terminals


def compute_unigram(trees):
    """Empirical unigram probabilities over lowercased leaves."""
    counts = defaultdict(int)
    total = 0
    for tree in trees:
        for w in tree.leaves():
            counts[w.lower()] += 1
            total += 1
    return {w: c / total for w, c in counts.items()}


# ============================================================
# 3. PCFGAnalyzer — torch-accelerated exact prefix / PMI
# ============================================================

class PCFGAnalyzer:
    """Compute exact prefix probabilities and PMI using PyTorch.

    Left-recursion is resolved via the left-corner matrix
    R_L = (I − P_L)^{−1}.  CKY rule application is vectorised
    over all binary rules using scatter_add_.
    """

    def __init__(self, rules, start_symbol, terminals, device='cpu'):
        self.rules = rules
        self.start_symbol = start_symbol
        self.terminals = terminals
        self.device = torch.device(device)

        # --- build non-terminal index ----------------------------------
        all_nts = set(rules.keys())
        for rhs_list in rules.values():
            for rhs, _ in rhs_list:
                for sym in rhs:
                    if sym.lower() not in terminals and sym not in terminals:
                        all_nts.add(sym)
        # keep only genuine non-terminals (exclude raw terminals)
        all_nts = {s for s in all_nts if s not in terminals}
        self.non_terminals = sorted(all_nts)
        self.M = len(self.non_terminals)
        self.nt2id = {s: i for i, s in enumerate(self.non_terminals)}

        assert start_symbol in self.nt2id, (
            f"Start symbol '{start_symbol}' not found among non-terminals. "
            f"First five NTs: {self.non_terminals[:5]}"
        )
        self.S_idx = self.nt2id[start_symbol]

        # --- pre-compute rule tensors ----------------------------------
        self._precompute_rule_tensors()
        # --- left-corner & unary matrices ------------------------------
        self._build_matrices()

    # ----------------------------------------------------------------
    def _is_nt(self, sym):
        return sym in self.nt2id

    def _precompute_rule_tensors(self):
        bin_a, bin_b, bin_c, bin_p = [], [], [], []
        un_a,  un_b,  un_p         = [], [], []
        self._term_rules = defaultdict(lambda: ([], []))

        for lhs, rhs_list in self.rules.items():
            if lhs not in self.nt2id:
                continue
            a = self.nt2id[lhs]
            for rhs, p in rhs_list:
                if len(rhs) == 1:
                    child = rhs[0]
                    # IMPORTANT: check NT *first* — POS tags (VERB, NOUN, …)
                    # collide with rare-word replacements (verb, noun, …)
                    # in the terminal set, so terminal-check must come second.
                    if self._is_nt(child):
                        un_a.append(a); un_b.append(self.nt2id[child]); un_p.append(p)
                    elif child in self.terminals or child.lower() in self.terminals:
                        key = child.lower() if child.lower() in self.terminals else child
                        self._term_rules[key][0].append(a)
                        self._term_rules[key][1].append(p)
                elif len(rhs) == 2:
                    b_sym, c_sym = rhs
                    if self._is_nt(b_sym) and self._is_nt(c_sym):
                        bin_a.append(a)
                        bin_b.append(self.nt2id[b_sym])
                        bin_c.append(self.nt2id[c_sym])
                        bin_p.append(p)

        dev = self.device
        self.bin_a = torch.tensor(bin_a, device=dev, dtype=torch.long)
        self.bin_b = torch.tensor(bin_b, device=dev, dtype=torch.long)
        self.bin_c = torch.tensor(bin_c, device=dev, dtype=torch.long)
        self.bin_p = torch.tensor(bin_p, device=dev, dtype=torch.float64)
        self.n_bin = len(bin_a)

        self.un_a = torch.tensor(un_a, device=dev, dtype=torch.long)
        self.un_b = torch.tensor(un_b, device=dev, dtype=torch.long)
        self.un_p = torch.tensor(un_p, device=dev, dtype=torch.float64)

        # convert terminal rule lists → tensors
        self.term_rules_t = {}
        for w, (idx_list, p_list) in self._term_rules.items():
            self.term_rules_t[w] = (
                torch.tensor(idx_list, device=dev, dtype=torch.long),
                torch.tensor(p_list,   device=dev, dtype=torch.float64),
            )

    def _build_matrices(self):
        M, dev = self.M, self.device

        # P_U  (unary non-terminal rules)
        P_U = torch.zeros(M, M, device=dev, dtype=torch.float64)
        if len(self.un_a) > 0:
            P_U.index_put_((self.un_a, self.un_b), self.un_p, accumulate=True)

        # P_L  = P_U + binary left-corner
        P_L = P_U.clone()
        if self.n_bin > 0:
            P_L.index_put_((self.bin_a, self.bin_b), self.bin_p, accumulate=True)

        I = torch.eye(M, device=dev, dtype=torch.float64)

        # Check spectral radius for numerical safety
        eigvals = torch.linalg.eigvals(P_L).abs()
        max_eig = eigvals.max().item()
        if max_eig >= 1.0:
            print(f"[WARN] P_L spectral radius = {max_eig:.6f} >= 1; "
                  "matrix inversion may be ill-conditioned.", file=sys.stderr)

        self.unary_closure = torch.linalg.inv(I - P_U)   # for inside (CKY)
        self.R_L           = torch.linalg.inv(I - P_L)   # for prefix probs

    # ----------------------------------------------------------------
    # Inside (CKY) algorithm — vectorised over rules
    # ----------------------------------------------------------------
    def compute_inside(self, words):
        """Compute inside table  inside[i, j, A] = P(A ⇒* w_i … w_j).

        *words* is a list of lowercased surface forms (after rare-word
        replacement).  Returns a torch tensor of shape (n, n, M).
        """
        n = len(words)
        M, dev = self.M, self.device
        inside = torch.zeros(n, n, M, device=dev, dtype=torch.float64)

        # --- span 1: terminal rules + unary closure --------------------
        for i, w in enumerate(words):
            if w in self.term_rules_t:
                idx, probs = self.term_rules_t[w]
                inside[i, i].scatter_add_(0, idx, probs)
            # unary closure
            inside[i, i] = self.unary_closure @ inside[i, i]

        # --- span ≥ 2: binary rules + unary closure --------------------
        if self.n_bin == 0:
            return inside

        for span in range(2, n + 1):
            num_cells = n - span + 1
            if num_cells == 0:
                continue

            for i in range(num_cells):
                j = i + span - 1
                cell = torch.zeros(M, device=dev, dtype=torch.float64)
                for k in range(i, j):
                    b_vals = inside[i, k][self.bin_b]       # (n_bin,)
                    c_vals = inside[k + 1, j][self.bin_c]   # (n_bin,)
                    contrib = self.bin_p * b_vals * c_vals   # (n_bin,)
                    cell.scatter_add_(0, self.bin_a, contrib)
                # unary closure on accumulated cell
                inside[i, j] = self.unary_closure @ cell

        return inside

    # ----------------------------------------------------------------
    # Prefix probability via left-corner matrix inversion
    # ----------------------------------------------------------------
    def compute_prefix_table(self, words, inside):
        """Compute prefix probability vectors for all sub-sequences.

        Returns prefix_table  where  prefix_table[(s, k)]  is a tensor
        of shape (M,) giving  P_prefix(A, w_s … w_k)  for every A.

        Computation order:  for each end-point k, sweep s = k … 0.
        """
        n = len(words)
        M, dev = self.M, self.device

        prefix_table = {}

        for k in range(n):
            for s in range(k, -1, -1):
                B = torch.zeros(M, device=dev, dtype=torch.float64)
                length = k - s + 1

                # terminal contribution (only when length == 1)
                if length == 1:
                    w = words[s]
                    if w in self.term_rules_t:
                        idx, probs = self.term_rules_t[w]
                        B.scatter_add_(0, idx, probs)

                # binary contributions: i in [1, length-1] ensures right_start <= k
                if self.n_bin > 0:
                    for i in range(1, length):
                        left_end    = s + i - 1
                        right_start = s + i
                        b_inside = inside[s, left_end][self.bin_b]
                        c_pfx    = prefix_table[(right_start, k)][self.bin_c]
                        contrib  = self.bin_p * b_inside * c_pfx
                        B.scatter_add_(0, self.bin_a, contrib)

                prefix_table[(s, k)] = self.R_L @ B

        return prefix_table

    # ----------------------------------------------------------------
    # PMI computation for a sentence
    # ----------------------------------------------------------------
    @torch.no_grad()
    def compute_pmi(self, words, unigram_probs):
        """Compute exact PMI(w_i ; w_{<i}) for each position.

        Parameters
        ----------
        words : list[str]
            Lowercased sentence (after rare-word replacement).
        unigram_probs : dict[str, float]
            Marginal word probabilities.

        Returns
        -------
        list[dict]   with keys word, P_u, P_c, surp_u, surp_c, pmi
        """
        n = len(words)
        inside = self.compute_inside(words)
        prefix_table = self.compute_prefix_table(words, inside)

        metrics = []
        for k in range(n):
            p_prefix_k    = prefix_table[(0, k)][self.S_idx].item()
            p_prefix_prev = (prefix_table[(0, k - 1)][self.S_idx].item()
                             if k > 0 else 1.0)

            p_c = (p_prefix_k / p_prefix_prev) if p_prefix_prev > 0 else 0.0
            p_u = unigram_probs.get(words[k], 1e-10)

            surp_u = -math.log2(p_u)           if p_u > 0 else float('inf')
            surp_c = -math.log2(p_c)           if p_c > 0 else float('inf')
            pmi    = surp_u - surp_c

            metrics.append({
                'word':   words[k],
                'P_u':    p_u,
                'P_c':    p_c,
                'surp_u': surp_u,
                'surp_c': surp_c,
                'pmi':    pmi,
            })
        return metrics


# ============================================================
# 4. Utilities
# ============================================================

def prepare_test_words(tree, vocab):
    """Return lowercased leaves, replacing OOV with POS tag."""
    words = []
    for pos in tree.treepositions('leaves'):
        w = tree[pos].lower()
        if w in vocab:
            words.append(w)
        else:
            parent = pos[:-1]
            words.append(tree[parent].label().lower())
    return words


def print_metrics(title, metrics):
    print(f"\n=== {title} ===")
    hdr = (f"{'Word':<20} | {'P(w)':<10} | {'P(w|ctx)':<12} | "
           f"{'Surp(w)':<10} | {'Surp(w|ctx)':<12} | {'PMI':<10}")
    print(hdr)
    print("-" * len(hdr))
    for m in metrics:
        sc = f"{m['surp_c']:12.4f}" if m['surp_c'] != float('inf') else f"{'inf':>12}"
        pm = f"{m['pmi']:10.4f}"    if abs(m['pmi']) != float('inf') else f"{'±inf':>10}"
        print(f"{m['word']:<20} | {m['P_u']:.6f} | {m['P_c']:12.8f} | "
              f"{m['surp_u']:10.4f} | {sc} | {pm}")


# ============================================================
# 5. Main
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description="Exact PMI via PCFG prefix probability (torch-accelerated)")
    parser.add_argument("--data", type=str, default="../data/converted_binary_trees.txt",
                        help="Path to newline-separated Penn-style binary trees")
    parser.add_argument("--train-ratio", type=float, default=0.9,
                        help="Fraction of trees used for training (rest = test)")
    parser.add_argument("--min-freq", type=int, default=2,
                        help="Minimum word frequency; rarer words → POS tag")
    parser.add_argument("--lexicalize", action="store_true",
                        help="Use lexicalised PCFG")
    parser.add_argument("--max-test-len", type=int, default=20,
                        help="Skip test sentences longer than this (CKY is O(n³))")
    parser.add_argument("--max-test-sents", type=int, default=5,
                        help="Number of test sentences to evaluate")
    parser.add_argument("--device", type=str, default=None,
                        help="'cpu' or 'cuda' (default: auto-detect)")
    args = parser.parse_args()

    # --- device --------------------------------------------------------
    if args.device is None:
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
    else:
        device = args.device
    print(f"Device: {device}")

    # --- load trees ----------------------------------------------------
    if not os.path.exists(args.data):
        sys.exit(f"Error: {args.data} not found.")

    with open(args.data, encoding="utf-8") as f:
        raw = [line.strip() for line in f if line.strip()]
    all_trees = [Tree.fromstring(t) for t in raw]
    print(f"Loaded {len(all_trees)} trees.")

    # --- train / test split --------------------------------------------
    n_train = int(len(all_trees) * args.train_ratio)
    train_trees = all_trees[:n_train]
    test_trees  = all_trees[n_train:]
    if not test_trees:
        test_trees = train_trees[:args.max_test_sents]
    print(f"Train: {len(train_trees)}  Test: {len(test_trees)}")

    # --- build vocabulary & replace rare words -------------------------
    freq = count_word_freq(train_trees)
    vocab = {w for w, c in freq.items() if c >= args.min_freq}
    print(f"Vocab size (freq >= {args.min_freq}): {len(vocab)}")

    train_trees_proc = [replace_rare_words_in_tree(t, vocab) for t in train_trees]

    # --- extract PCFG --------------------------------------------------
    t0 = time.time()
    rules, start_sym, terminals = extract_pcfg(
        train_trees_proc, lexicalize=args.lexicalize)
    n_rules = sum(len(v) for v in rules.values())
    print(f"PCFG: {len(rules)} non-terminals, {n_rules} rules, "
          f"start = {start_sym}  ({time.time()-t0:.2f}s)")

    # --- unigram from processed training trees -------------------------
    unigram = compute_unigram(train_trees_proc)

    # --- build analyser ------------------------------------------------
    t0 = time.time()
    analyser = PCFGAnalyzer(rules, start_sym, terminals, device=device)
    print(f"Analyser built ({analyser.M} NTs on {device},"
          f" {time.time()-t0:.2f}s)")

    # --- evaluate on test sentences ------------------------------------
    evaluated = 0
    for idx, tree in enumerate(test_trees):
        if evaluated >= args.max_test_sents:
            break

        words = prepare_test_words(tree, vocab)
        if len(words) > args.max_test_len:
            continue

        print(f"\n{'='*60}")
        print(f"Test sentence {idx}: {' '.join(words)}  (len={len(words)})")

        t0 = time.time()
        metrics = analyser.compute_pmi(words, unigram)
        elapsed = time.time() - t0

        label = "Lexicalised" if args.lexicalize else "Unlexicalised"
        print_metrics(f"{label} PCFG  ({elapsed:.2f}s)", metrics)
        evaluated += 1

    if evaluated == 0:
        print("No test sentences within --max-test-len.  "
              "Try increasing the limit.")


if __name__ == "__main__":
    main()
