import os
import re
from typing import Iterator, Optional
import copy
import csv
import pandas as pd

SLASH = re.compile(r"([/\\])")
CAT_SPLIT = re.compile(r"([/\\]|[\[\]\(\)/\\])")


def inverse_dic(dictionary: dict):
    return {v: k for k, v in dictionary.items()}


class Feature:
    def __init__(self, value: Optional[str] = None):
        self.value: Optional[str] = value

    def __str__(self) -> str:
        return self.value if self.value else ""

    def __eq__(self, other: "Feature") -> bool:
        assert isinstance(
            other, Feature
        ), f"Feature is being compared with {type(object)}."
        return not self.value or not other.value or self.value == other.value

    def __hash__(self):
        return hash(self.value)


class Category:
    def __truediv__(self, other: "Category") -> "Category":
        return Complex(self, "/", other)

    def __or__(self, other: "Category") -> "Category":
        return Complex(self, "\\", other)

    @property
    def is_complex(self):
        return not self.is_basic

    @property
    def is_basic(self):
        return not self.is_complex

    @classmethod
    def from_string(cls, txt: str) -> "Category":
        # S[dcl][conj]のような素性を排除
        if "[conj]" in txt:
            txt = txt.replace("[conj]", "")
        if r"((S\NP)\(S\NP))/((S\NP)\(S\NP))[dcl]" in txt:
            txt = txt.replace(
                r"((S\NP)\(S\NP))/((S\NP)\(S\NP))[dcl]",
                r"((S\NP)\(S\NP))/((S\NP)\(S\NP))",
            )
        tokens = CAT_SPLIT.sub(r" \1 ", txt)
        buffer = list(reversed([i for i in tokens.split(" ") if i != ""]))
        stack = []

        while len(buffer):
            item = buffer.pop()
            if item in "(":
                stack.append(item)
            elif item in ")":
                y = stack.pop()
                assert len(stack) > 0
                if stack[-1] == "(" and item == ")":
                    assert stack.pop() in "("
                    stack.append(y)
                else:
                    f = stack.pop()
                    x = stack.pop()
                    assert stack.pop() in "("
                    stack.append(Complex(x, f, y))
            elif SLASH.match(item):
                stack.append(item)
            else:
                if len(buffer) >= 3 and buffer[-1] == "[":
                    buffer.pop()
                    feature = Feature(buffer.pop())
                    assert buffer.pop() == "]"
                    stack.append(Basic(item, feature))
                else:
                    stack.append(Basic(item))

        if len(stack) == 1:
            return stack[0]
        try:
            x, f, y = stack
            return Complex(x, f, y)
        except ValueError:
            raise RuntimeError(f"falied to parse category: {txt}")

    def clean_feature(self) -> None:
        def _rec(cat: Category) -> Category:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    cat.feature = None

        _rec(self)

    @property
    def without_feature(self) -> "Category":
        cat_copy = copy.deepcopy(self)

        def _rec(cat: Category) -> None:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    cat.feature = None

        _rec(cat_copy)
        return cat_copy

    @property
    def features(self) -> list[str]:
        def _rec(cat: Category) -> None:
            if cat.is_complex:
                _rec(cat.left)
                _rec(cat.right)
            else:
                if isinstance(cat.feature, Feature):
                    result.append(str(cat.feature))

        result: list[str] = []
        _rec(self)
        return result

    @property
    def is_vp(cat: "Category") -> bool:
        """
        An English functor category is a VP when
            1. it includes just one 'S' category and
            2. it starts with 'S' category and
        """
        s = str(cat)
        return cat.is_complex and s.count("S") == 1 and re.match(r"\(*S", s) is not None


class Basic(Category):
    def __init__(self, base: str, feature: Optional[Feature] = None):
        self.base: str = base
        self.feature: Optional[Feature] = feature
        self.t: Optional[Category] = None

    def __str__(self) -> str:
        if self.feature:
            return f"{self.base}[{self.feature}]"
        return self.base

    def __eq__(self, other: object) -> bool:
        if isinstance(other, str):
            other = Category.from_string(other)
        if isinstance(other, Complex):
            return False
        if self.feature and other.feature:
            return self.base == other.base and self.feature and other.feature
        else:
            return self.base == other.base

    def __xor__(self, other: object) -> bool:
        if not isinstance(other, Basic):
            return False
        return self.base == other.base

    def __hash__(self) -> int:
        return hash(str(self))

    @property
    def is_basic(self):
        return True

    @property
    def is_modifier(self) -> bool:
        return False

    @property
    def is_post_modifier(self) -> bool:
        return False

    @property
    def nargs(self) -> int:
        return 0

    @property
    def to_latex(self) -> str:
        if self.feature:
            return f"{self.base}\\[{self.feature}\\]"
        return self.base


class Complex(Category):
    def __init__(self, left: str | Category, slash: str, right: str | Category):
        self.left: Category = (
            Category.from_string(left) if isinstance(left, str) else left
        )
        self.slash: str = slash
        self.right: Category = (
            Category.from_string(right) if isinstance(right, str) else right
        )

    def __str__(self) -> str:
        def _str(cat):
            if isinstance(cat, Complex):
                return f"({cat})"
            return str(cat)

        return _str(self.left) + self.slash + _str(self.right)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, str):
            other = Category.from_string(other)
        elif not isinstance(other, Complex):
            return False
        return (
            self.left == other.left
            and self.slash == other.slash
            and self.right == other.right
        )

    def __xor__(self, other: object) -> bool:
        if not isinstance(other, Complex):
            return False
        return (
            self.left ^ other.left
            and self.slash == other.slash
            and self.right ^ other.right
        )

    def __hash__(self) -> int:
        return hash(str(self))

    @property
    def is_complex(self):
        return True

    @property
    def is_modifier(self) -> bool:
        return self.left == self.right and self.slash == "/"

    @property
    def is_post_modifier(self) -> bool:
        return self.left == self.right and self.slash == "\\"

    @property
    def nargs(self) -> int:
        return 1 + self.left.nargs


# constraint
ROOT_CATS: set[Category] = {
    Category.from_string("NP[nc]"),
    Category.from_string("NP"),
    Category.from_string("S"),
}


PUNC: set[Category] = {
    Category.from_string("."),
    Category.from_string(","),
    Category.from_string(";"),
    Category.from_string(":"),
    Category.from_string("LRB"),
    Category.from_string("RRB"),
}
CONJ: Category = Category.from_string("conj")


def fa(left: Category, right: Category) -> Optional[Category]:
    if left.is_complex and left.right == right and left.slash == "/":
        left_deepcopy = copy.deepcopy(left)
        return left_deepcopy.left
    return


def ba(left: Category, right: Category) -> Optional[Category]:
    if right.is_complex and left == right.right and right.slash == "\\":
        right_deepcopy = copy.deepcopy(right)
        return right_deepcopy.left
    return


def fc(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.right == right.left
        and left.slash == right.slash == "/"
    ):
        return Complex(
            left.left,
            "/",
            right.right,
        )
    return


def fc2(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 2
        and left.right == right.left.left
        and left.slash == right.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left = left.left
        return result
    return


def fc3(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 3
        and left.right == right.left.left.left
        and left.slash == right.left.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left.left = left.left
        return result
    return


def fc4(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 4
        and left.right == right.left.left.left.left
        and left.slash == right.left.left.left.slash == "/"
    ):
        result = copy.deepcopy(right)
        result.left.left.left.left = left.left
        return result
    return


def fcx(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.right == right.left
        and left.slash == "/"
        and right.slash == "\\"
    ):
        return Complex(
            left.left,
            "\\",
            right.right,
        )
    return


def fc2x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 2
        and left.right == right.left.left
        and left.slash == "/"
        and right.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left = left.left
        return result
    return


def fc3x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 3
        and left.right == right.left.left.left
        and left.slash == "/"
        and right.left.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left.left = left.left
        return result
    return


def fc4x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.nargs >= 4
        and left.right == right.left.left.left.left
        and left.slash == "/"
        and right.left.left.left.slash == "\\"
    ):
        result = copy.deepcopy(right)
        result.left.left.left.left = left.left
        return result
    return


def bc(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.left == right.right
        and right.slash == left.slash == "\\"
    ):
        return Complex(
            right.left,
            "\\",
            left.right,
        )
    return


def bc2(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 2
        and right.is_complex
        and left.left.left == right.right
        and right.slash == left.left.slash == "\\"
    ):
        result = copy.deepcopy(left)
        result.left.left = right.left
        return result
    return


def bc3(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 3
        and right.is_complex
        and left.left.left.left == right.right
        and right.slash == left.left.left.slash == "\\"
    ):
        result = copy.deepcopy(left)
        result.left.left.left = right.left
        return result
    return


def bcx(left: Category, right: Category) -> Optional[Category]:
    if (
        left.is_complex
        and right.is_complex
        and left.left == right.right
        and right.slash == "\\"
        and left.slash == "/"
    ):
        return Complex(
            right.left,
            "/",
            left.right,
        )
    return


def bc2x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 2
        and right.is_complex
        and left.left.left == right.right
        and right.slash == "\\"
        and left.left.slash == "/"
    ):
        result = copy.deepcopy(left)
        result.left.left = right.left
        return result
    return


def bc3x(left: Category, right: Category) -> Optional[Category]:
    if (
        left.nargs >= 3
        and right.is_complex
        and left.left.left.left == right.right
        and right.slash == "\\"
        and left.left.left.slash == "/"
    ):
        result = copy.deepcopy(left)
        result.left.left.left = right.left
        return result
    return


def punc(left: Category, right: Category) -> Optional[Category]:
    if right in PUNC:
        return left
    elif left in PUNC:
        return right
    return


def conj(left: Category, right: Category) -> Optional[Category]:
    if left == CONJ:
        return right


COMBINATORS: dict = {
    fa: ">",
    ba: "<",
    fc: ">B",
    fc2: ">B2",
    fc3: ">B3",
    fc4: ">B4",
    fcx: ">Bx1",
    fc2x: ">Bx2",
    fc3x: ">Bx3",
    fc4x: ">Bx4",
    bc: "<B1",
    bc2: "<B2",
    bc3: "<B3",
    bcx: "<Bx",
    bc2x: "<Bx2",
    bc3x: "<Bx3",
    punc: "punc",
    conj: ">",  # corresponds to conj
}


def binary_comp(
    left: Category, right: Category
) -> tuple[Optional[Category], Optional[str]]:
    for combinator in COMBINATORS:
        cat: Optional[Category] = combinator(left, right)
        if cat:
            return cat, COMBINATORS[combinator]
        else:
            continue
    return None, None


class Tree:
    def __init__(
        self,
        cat: Category,
        children: Optional[list["Tree"]],
        comb: str = "lex",
        token: Optional[str] = None,
    ) -> None:
        assert children != token, "両方Noneはだめ"
        self.cat = cat
        self.children = children
        self.comb = comb
        self.token = token

    @property
    def leaves(self) -> list["Tree"]:
        def rec(tree: "Tree") -> None:
            assert isinstance(tree, Tree), f"{tree=}"
            if tree.children:
                for child in tree.children:
                    rec(child)
            else:
                result.append(tree)

        result: list["Tree"] = []
        rec(self)
        return result

    @property
    def tokens(self) -> list[str]:
        return [leaf.token for leaf in self.leaves if isinstance(leaf.token, str)]

    @property
    def rightmost_token(self) -> str:
        if self.is_terminal:
            return self.token
        return self.tokens[-1]

    @property
    def terminal_cat(self) -> list[str]:
        return [str(leaf.cat) for leaf in self.leaves]

    @property
    def terminal_token_cat(self) -> list[tuple[str, Category]]:
        return [
            (leaf.token, leaf.cat)
            for leaf in self.leaves
            if isinstance(leaf.token, str)
        ]

    @property
    def is_terminal(self) -> bool:
        return isinstance(self.token, str)

    @property
    def word(self) -> str:
        return " ".join(token for token in self.tokens)

    @property
    def left(self) -> "Tree":
        assert self.is_binary
        assert self.children
        return self.children[0]

    @property
    def right(self) -> "Tree":
        assert self.is_binary
        assert self.children
        return self.children[1]

    @property
    def child(self) -> "Tree":
        assert self.is_unary
        assert self.children
        return self.children[0]

    @property
    def is_unary(self) -> bool:
        return isinstance(self.children, list) and len(self.children) == 1

    @property
    def is_binary(self) -> bool:
        return isinstance(self.children, list) and len(self.children) == 2

    @property
    def is_root(self) -> bool:
        return self.cat in ROOT_CATS

    @staticmethod
    def comp(left: Optional["Tree"], right: Optional["Tree"]) -> Optional["Tree"]:
        if left and right:
            cat, comb = binary_comp(left.cat, right.cat)
            if cat and comb:
                return Tree(cat, [left, right], comb)
        return


def typeraise(left: Category, right: Complex) -> Complex:
    return Complex(right.left, "/", Complex(right.left, "\\", left))


def en_apply_typeraise(tree: Tree) -> Tree:
    def _apply_typeraise(node: Tree) -> Tree:
        if node.is_terminal:
            return Tree(node.cat, None, "lex", node.token)
        elif node.is_unary:
            return Tree(
                node.cat,
                [_apply_typeraise(node.child)],
                node.comb,
            )
        elif node.comb == "bi" and ba(node.left.cat, node.right.cat):
            return Tree(
                node.cat,
                [
                    Tree(
                        typeraise(node.left.cat, node.right.cat),
                        [_apply_typeraise(node.left)],
                        "uni",
                    ),
                    _apply_typeraise(node.right),
                ],
                "bi",
            )
        else:
            return Tree(
                node.cat,
                [_apply_typeraise(node.left), _apply_typeraise(node.right)],
                node.comb,
            )

    return _apply_typeraise(tree)


def rotate2left(tree: Tree) -> Tree:
    def _rotate2left(node: Tree) -> Tree:
        if node.is_terminal:
            return Tree(node.cat, None, "lex", node.token)
        elif node.is_unary:
            return Tree(node.cat, [_rotate2left(node.children[0])], node.comb)
        else:  # node.is_binary
            if node.right.is_binary:
                new_node: Optional[Tree] = Tree.comp(
                    Tree.comp(node.left, node.right.left), node.right.right
                )
                if new_node:
                    return _rotate2left(new_node)
                return Tree(
                    node.cat,
                    [_rotate2left(node.left), _rotate2left(node.right)],
                    node.comb,
                )
            return Tree(
                node.cat,
                [_rotate2left(node.left), _rotate2left(node.right)],
                node.comb,
            )

    return _rotate2left(tree)


def printer(tree: Tree) -> str:
    # Output tree strings of the Japanese CCGBank's format
    def _rec(node: Tree):
        if node.is_terminal:
            cat = node.cat
            word = node.word

            return f"{{{cat} {word}/_/_/_}}"
        else:
            children = " ".join(_rec(child) for child in node.children)
            return f"{{{node.comb} {node.cat} {children}}}"

    return _rec(tree)


stack: list[Tree] = []


class AutoLineReader:
    def __init__(self, line: str):
        self.line: str = line
        self.index: int = 0
        self.word_id: int = -1
        self.binary_comp = binary_comp
        self.tokens: list[str] = []

    def _next(self) -> str:
        end: int = self.line.find(" ", self.index)
        res: str = self.line[self.index : end]
        self.index = end + 1
        return res

    def _check(self, text: str, offset=0) -> None:
        if self.line[self.index + offset] != text:
            raise RuntimeError(f"failed to parse: {self.line}")

    def _peek(self) -> str:
        return self.line[self.index]

    def parse(self) -> Tree:
        tree = self._next_node()
        return tree

    @property
    def _next_node(self):
        if self.line[self.index + 2] == "L":
            return self._parse_leaf
        elif self.line[self.index + 2] == "T":
            return self._parse_tree
        else:
            raise RuntimeError(
                f"failed to parse:\n{self.index + 2=}\n{self.line[self.index + 2]=}\n{self.line=}"
            )

    def _parse_leaf(self) -> Tree:
        self.word_id += 1
        self._check("(")
        self._check("<", 1)
        self._check("L", 2)
        self._next()
        cat: Category = Category.from_string(self._next())
        self._next()
        self._next()
        # tag1 = self.next()  # modified POS tag
        # tag2 = self.next()  # original POS
        token: str = self._next().replace("\\", "")
        if token == "(":
            token = "LRB"
        elif token == ")":
            token = "RRB"
        self.tokens.append(token)
        self._next()
        return Tree(cat, None, "lex", token)

    def _parse_tree(self) -> Tree:
        self._check("(")
        self._check("<", 1)
        self._check("T", 2)
        self._next()
        cat: Category = Category.from_string(self._next())
        self._next()
        self._next()
        children: list[Tree] = []
        while self._peek() != ")":
            children.append(self._next_node())
        self._next()
        if len(children) == 2:
            left, right = children
            return Tree(cat, [left, right], "bi")

        elif len(children) == 1:
            return Tree(cat, children, "uni")
        else:
            raise RuntimeError(f"failed to parse:\n{children=}\n{self.line=}")


def read_auto(filename: str) -> Iterator[Tree]:
    for line in open(filename):
        line = line.strip()
        if len(line) == 0:
            continue
        tree = AutoLineReader(line).parse()
        yield tree

def bottom_up_effort(tree: Tree, effort: int = 0) -> list[int]:
    if not tree.is_terminal:
        return [
            c for child in tree.children[:-1] for c in bottom_up_effort(child)
        ] + bottom_up_effort(tree.children[-1], effort + 1)
    else:
        return [effort]


def bottomup_shift(tree: Tree) -> list[int]:
    def traverse(node: Tree) -> None:
        if node.is_terminal:
            comb_list.append(node.comb)
        else:
            if node.is_unary:
                traverse(node.children[0])
                comb_list.append(node.comb)
            else:
                traverse(node.children[0])
                traverse(node.children[1])
                comb_list.append(node.comb)

    comb_list: list[str] = []
    traverse(tree)

    shift_count: int = 0
    result: list[int] = []
    for comb in comb_list:
        if comb == "lex":
            result.append(shift_count)
            shift_count = 0
        elif comb == "uni":
            shift_count += 1
        else:
            continue
    result.append(shift_count)
    return result[1:]


def make_nodecount_data(input_path: str, output_path: str) -> None:
    rightbranch_nc: list[int] = []
    rightbranch_shift: list[int] = []
    leftbranch_nc: list[int] = []
    leftbranch_shift: list[int] = []
    tokens: list[str] = []
    itemnos: list[str] = []

    for i, tree in enumerate(read_auto(input_path), start=1):
        words = tree.tokens
        words: list[str] = [
            word.replace("-LRB-", "(")
            .replace("-RRB-", ")")
            .replace("-LSB-", "[")
            .replace("-RSB-", "]")
            .replace("``", "''")
            for word in words
        ]
        rightbranch_token_nc = bottom_up_effort(tree)
        rightbranch_token_shift = bottomup_shift(tree)
        tree = rotate2left(tree)
        leftbranch_token_nc = bottom_up_effort(tree)
        leftbranch_token_shift = bottomup_shift(tree)
        assert len(words) == len(rightbranch_token_nc) == len(leftbranch_token_nc)

        tokens += words
        rightbranch_nc += rightbranch_token_nc
        rightbranch_shift += rightbranch_token_shift
        leftbranch_nc += leftbranch_token_nc
        leftbranch_shift += leftbranch_token_shift
        itemnos += [i] * len(words)

    rows = [
        {
            "token": tokens[i],
            "Itemno": itemnos[i],
            "rightbranch_nc": rightbranch_nc[i],
            "rightbranch_shift": rightbranch_shift[i],
            "leftbranch_nc": leftbranch_nc[i],
            "leftbranch_shift": leftbranch_shift[i],
        }
        for i in range(len(tokens))
    ]

    with open(output_path, "w", newline="") as file:
        writer = csv.DictWriter(
            file,
            delimiter="\t",
            fieldnames=[
                "token",
                "Itemno",
                "rightbranch_nc",
                "rightbranch_shift",
                "leftbranch_nc",
                "leftbranch_shift",
            ],
        )
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    make_nodecount_data(
        "./data/OneStop/ccg.txt",
        "./data/os_ccg.tsv",
    )
