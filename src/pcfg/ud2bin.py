import os
import glob
from nltk.tree import Tree

def escape_ptb_token(token):
    mapping = {
        "(": "-LRB-",
        ")": "-RRB-",
        "[": "-LSB-",
        "]": "-RSB-",
        "{": "-LCB-",
        "}": "-RCB-",
        " ": "_" 
    }
    for k, v in mapping.items():
        token = token.replace(k, v)
    return token

def parse_conllu_sentence(lines):
    nodes = {}
    children_map = {}

    for line in lines:
        line = line.strip()
        if not line or line.startswith("#"):
            continue

        parts = line.split("\t")
        if not parts[0].isdigit():
            continue

        token_id = int(parts[0])

        form = escape_ptb_token(parts[1])
        pos = escape_ptb_token(parts[3])

        head_id = int(parts[6])

        nodes[token_id] = {"id": token_id, "form": form, "pos": pos, "head": head_id}

        if head_id not in children_map:
            children_map[head_id] = []
        children_map[head_id].append(token_id)

    return nodes, children_map


def is_projective(nodes):
    arcs = [
        (nodes[i]["head"], i)
        for i in nodes
        if nodes[i]["head"] != 0
    ]
    for idx, (h1, d1) in enumerate(arcs):
        lo1, hi1 = min(h1, d1), max(h1, d1)
        for h2, d2 in arcs[idx + 1:]:
            lo2, hi2 = min(h2, d2), max(h2, d2)
            if lo1 < lo2 < hi1 < hi2:
                return False
            if lo2 < lo1 < hi2 < hi1:
                return False
    return True

def build_strict_binary_tree(node_id, nodes, children_map):
    node = nodes[node_id]

    current_tree = Tree(node["pos"], [node["form"]])

    children_ids = children_map.get(node_id, [])

    left_deps = sorted([cid for cid in children_ids if cid < node_id], reverse=True)
    right_deps = sorted([cid for cid in children_ids if cid > node_id])

    for right_id in right_deps:
        dep_tree = build_strict_binary_tree(right_id, nodes, children_map)
        current_tree = Tree(node["pos"], [current_tree, dep_tree])

    for left_id in left_deps:
        dep_tree = build_strict_binary_tree(left_id, nodes, children_map)
        current_tree = Tree(node["pos"], [dep_tree, current_tree])
        
    return current_tree

def _process_sentence_lines(lines, out_f, stats):
    nodes, children_map = parse_conllu_sentence(lines)

    root_ids = children_map.get(0, [])
    if not root_ids:
        stats["skipped_noroot"] += 1
        return

    if not is_projective(nodes):
        stats["skipped_nonproj"] += 1
        return

    root_id = root_ids[0]
    binary_tree = build_strict_binary_tree(root_id, nodes, children_map)
    final_tree = Tree("ROOT", [binary_tree])

    tree_str = " ".join(str(final_tree).split())
    out_f.write(tree_str + "\n")
    stats["processed"] += 1


def convert_conllu_to_binary_ptb(input_folder, output_file):
    conllu_files = glob.glob(os.path.join(input_folder, "*.conllu"))
    if not conllu_files:
        return

    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    stats = {"processed": 0, "skipped_nonproj": 0, "skipped_noroot": 0}

    with open(output_file, "w", encoding="utf-8") as out_f:
        for file_path in sorted(conllu_files):
            with open(file_path, "r", encoding="utf-8") as in_f:
                current_sentence_lines = []
                for line in in_f:
                    if line.strip() == "":
                        if current_sentence_lines:
                            _process_sentence_lines(current_sentence_lines, out_f, stats)
                            current_sentence_lines = []
                    else:
                        current_sentence_lines.append(line)

                if current_sentence_lines:
                    _process_sentence_lines(current_sentence_lines, out_f, stats)

    print(f"  Processed      : {stats['processed']:>6} sentences -> {output_file}")
    print(f"  Skipped (non-projective): {stats['skipped_nonproj']:>6}")
    print(f"  Skipped (no root)       : {stats['skipped_noroot']:>6}")


if __name__ == "__main__":
    INPUT_FOLDER_PATH = "../../data/UD_English-GUM"
    OUTPUT_FILE_PATH = "../../data/binary_trees.txt"
    
    convert_conllu_to_binary_ptb(INPUT_FOLDER_PATH, OUTPUT_FILE_PATH)
