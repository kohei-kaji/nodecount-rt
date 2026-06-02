import re
import csv


def top_down_effort(line: str) -> list[int]:
    td: list[int] = []
    rspines: list[str] = line.split(") ")
    for rspine in rspines:
        td.append(rspine.count("("))
    return td


def bottom_up_effort(line: str) -> list[int]:
    bu: list[int] = []
    nodes: list[str] = line.split(" ")
    for node in nodes:
        if ")" in node:
            bu.append(node.count(")"))
    return bu


def parse_tree(tree_str: str):
    tree_str = tree_str.strip()
    tokens = re.findall(r"\(|\)|[^\s()]+", tree_str)

    def read_expression(idx: int = 0):
        assert tokens[idx] == "("
        idx += 1

        label = tokens[idx]
        idx += 1

        children = []
        while tokens[idx] != ")":
            if tokens[idx] == "(":
                subtree, idx = read_expression(idx)
                children.append(subtree)
            else:
                children.append(tokens[idx])
                idx += 1

        idx += 1
        return [label] + children, idx

    parsed_tree, _ = read_expression(0)
    return parsed_tree

def tree_to_inorder_actions(tree) -> list[str]:
    actions = []

    def traverse_inorder(node):
        if isinstance(node, str):
            actions.append("SHIFT")
            return

        label = node[0]
        children = node[1:]

        if len(children) > 0:
            traverse_inorder(children[0])

        actions.append(f"NT({label})")

        for c in children[1:]:
            traverse_inorder(c)

        actions.append("REDUCE")

    traverse_inorder(tree)
    return actions


def left_corner_effort(line: str) -> list[int]:
    parsed_tree = parse_tree(line)
    actions: list[str] = tree_to_inorder_actions(parsed_tree)

    node_count = 0
    results: list[int] = []

    for action in actions:
        if action.startswith("NT("):
            node_count += 1
        elif action == "SHIFT":
            results.append(node_count)
            node_count = 0
        elif action == "REDUCE":
            pass
    results.append(node_count)

    return results[1:]


class Tree:
    def __init__(self, pos: str, children: list["Tree"] | list[str]) -> None:
        self.pos = pos
        self.children = children
        assert len(self.children) > 0, "children must contain some Tree|str"

    def __len__(self) -> int:
        return len(self.leaves)

    @property
    def leaves(self) -> list["Tree"]:
        def _rec(node: Tree) -> None:
            if node.is_terminal:
                result.append(node)
            else:
                for child in node.children:
                    _rec(child)

        result: list["Tree"] = []
        _rec(self)
        return result

    @property
    def token(self) -> str:
        assert self.is_terminal, "Tree.token must be called on terminal objects"
        token = self.children[0]
        return token

    @property
    def tokens(self) -> list[str]:
        return [leaf.token for leaf in self.leaves]

    @property
    def is_terminal(self) -> bool:
        assert self.children != [], f"An empty list is detected: pos = {self.pos}"
        return isinstance(self.children[0], str)


class PTBReader:
    def __init__(self, line: str) -> None:
        self.line: str = line
        self.index: int = 0
        self.tokens = []

    def _next(self, target: str) -> str:
        start: int = self.index
        end: int = self.line.find(target, self.index)
        result: str = self.line[start:end]
        self.index: int = end + 1
        return result

    def _is_current_idx(self, text: str) -> None:
        if self.line[self.index] != text:
            raise RuntimeError("the position of 'index' is not correct")

    def peek_current_index_str(self) -> str:
        return self.line[self.index]

    def parse(self) -> Tree:
        return self._next_node()

    @property
    def _next_node(self):
        end = self.line.find(" ", self.index)
        if self.line[end + 1] == "(":
            return self.parse_tree
        else:
            return self.parse_terminal

    def parse_terminal(self) -> Tree:
        self._is_current_idx("(")
        pos = self._next(" ")[1:]
        token = self._next(")")
        self.tokens.append(token)
        return Tree(pos, [token])

    def parse_tree(self) -> Tree:
        self._is_current_idx("(")
        pos = self._next(" ")[1:]
        self._is_current_idx("(")

        children = []
        while self.peek_current_index_str() != ")":
            children.append(self._next_node())
            if self.peek_current_index_str() == " ":
                self._next(" ")

        self._next(")")

        return Tree(pos, children)


def make_nodecount_data(input_path: str, output_path: str) -> None:
    topdown: list[int] = []
    bottomup: list[int] = []
    leftcorner: list[int] = []
    words: list[str] = []
    itemnos: list[str] = []

    with open(input_path, "r") as f:
        for i, line in enumerate(f, start=1):
            line = line.strip()
            assert (
                sum(top_down_effort(line))
                == sum(bottom_up_effort(line))
                == sum(left_corner_effort(line))
            ), f"\n{line}\n{top_down_effort(line)=}\n{bottom_up_effort(line)=}\n{left_corner_effort(line)=}"
            topdown += top_down_effort(line)
            bottomup += bottom_up_effort(line)
            leftcorner += left_corner_effort(line)

            tokens: list[str] = [
                token.replace("-LRB-", "(")
                .replace("-RRB-", ")")
                .replace("-LSB-", "[")
                .replace("-RSB-", "]")
                for token in PTBReader(line).parse().tokens
            ]
            words += tokens
            itemnos += [i] * len(tokens)

    assert (len(topdown) == len(bottomup) == len(leftcorner) == len(words) == len(itemnos))
    assert (sum(topdown) == sum(bottomup) == sum(leftcorner)), f"\n{sum(topdown)=}\n{sum(bottomup)=}\n{sum(leftcorner)=}"

    rows = [
        {
            "word": words[i],
            "Itemno": itemnos[i],
            "topdown_nc": topdown[i] - 1,
            "bottomup_nc": bottomup[i] - 1,
            "leftcorner_nc": leftcorner[i] - 1,
        }
        for i in range(len(words))
    ]

    with open(output_path, "w", newline="") as file:
        writer = csv.DictWriter(
            file,
            delimiter="\t",
            fieldnames=["word", "Itemno", "topdown_nc", "bottomup_nc", "leftcorner_nc"],
        )
        writer.writeheader()
        writer.writerows(rows)

if __name__ == "__main__":
    make_nodecount_data(
        "./data/OneStop/benepar.txt",
        "./data/os_cfg.tsv",
    )
