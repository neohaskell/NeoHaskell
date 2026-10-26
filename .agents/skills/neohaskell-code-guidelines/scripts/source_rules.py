"""Finite source-name and explicit-signature checks that supplement HLint."""

from __future__ import annotations

from dataclasses import dataclass
import re


NAME = r"(?:[A-Z][\w']*\.)*[A-Za-z_][\w']*"
TOKEN = re.compile(NAME + r"|->|=>|::|\.\.|[^\s]")
IDENTIFIER = re.compile(NAME)
CHARACTER = re.compile(r"'(?:[^'\\\n]|\\(?:[A-Za-z0-9]+|.))'")
QUASIQUOTE = re.compile(r"\[" + NAME + r"\|")
LINE_COMMENT = re.compile(r"--+(?![!#$%&*+./<=>?@\\^|~:-])")
PROVIDERS = {
    "LinkedList": {"LinkedList", "Core"},
    "Char": {"Char", "Core", "Prelude", "GHC.Types"},
    "True": {"Basics", "Core", "Prelude", "GHC.Types", "Data.Bool"},
    "False": {"Basics", "Core", "Prelude", "GHC.Types", "Data.Bool"},
    "fmt": {"Basics", "Core"},
}
CONVERSIONS = {
    ("core/core/Text.hs", "Text", "fromLinkedList"),
    ("core/system/Path.hs", "Path", "fromLinkedList"),
}


def code_source(source, path):
    if not str(path).endswith(".lhs"):
        return source
    lines = []
    inside = False
    for line in source.splitlines(keepends=True):
        if line.strip() in {r"\begin{code}", r"\end{code}"}:
            inside = line.strip() == r"\begin{code}"
            lines.append("\n" if line.endswith("\n") else "")
        elif line.startswith(">"):
            lines.append(line[1:].removeprefix(" "))
        elif inside:
            lines.append(line)
        else:
            lines.append("\n" if line.endswith("\n") else "")
    return "".join(lines)


def source_parts(source):
    masked = list(source)
    pragmas = []
    position = 0
    while position < len(source):
        start = position
        if source.startswith("{-#", position):
            end = source.find("#-}", position + 3)
            if end < 0:
                break
            position = end + 3
            pragmas.append((start, position, source[start:position]))
        elif source.startswith("{-", position):
            depth = 1
            position += 2
            while position < len(source) and depth:
                if source.startswith("{-", position):
                    depth += 1
                    position += 2
                elif source.startswith("-}", position):
                    depth -= 1
                    position += 2
                else:
                    position += 1
        elif LINE_COMMENT.match(source, position):
            position = source.find("\n", position)
            if position < 0:
                position = len(source)
        elif source[position] == '"':
            position += 1
            while position < len(source):
                if source[position] == "\\":
                    position += 2
                elif source[position] == '"':
                    position += 1
                    break
                else:
                    position += 1
        else:
            literal = CHARACTER.match(source, position)
            quote = QUASIQUOTE.match(source, position)
            if literal:
                position = literal.end()
            elif quote:
                end = source.find("|]", quote.end())
                position = len(source) if end < 0 else end + 2
            else:
                identifier = IDENTIFIER.match(source, position)
                position = identifier.end() if identifier else position + 1
                continue
        for index in range(start, min(position, len(source))):
            if source[index] != "\n":
                masked[index] = " "
    return "".join(masked), pragmas


@dataclass(frozen=True)
class ImportedNames:
    module: str
    qualifier: str
    qualified: bool
    names: frozenset | None
    hidden: bool

    def contains(self, name):
        if self.names is None:
            return True
        listed = name in self.names or (name in {"True", "False"} and {"Bool", ".."} <= self.names)
        return not listed if self.hidden else listed


class SourceNames:
    def __init__(self, source):
        self.masked, self.pragmas = source_parts(source)
        self.masked = self.masked.expandtabs(8)
        module = re.search(r"(?ms)^[ \t]*module\s+([A-Z][\w'.]*).*?\bwhere\b", self.masked)
        self.module = module[1] if module else "Main"
        header_end = module.end() if module else 0
        body = self.masked[header_end:].splitlines(keepends=True)
        indents = [len(line) - len(line.lstrip(" \t")) for line in body if line.strip() and not line.lstrip().startswith("#")]
        margin = min(indents, default=0)
        self.masked = self.masked[:header_end] + "".join(line[margin:] if line.strip() and not line.lstrip().startswith("#") else line for line in body)
        self.imports = []
        for match in re.finditer(r"(?ms)^import\b(.*?)(?=^\S|\Z)", self.masked):
            tokens = TOKEN.findall(match[1])
            if not tokens:
                continue
            qualified = "qualified" in tokens
            tokens = [token for token in tokens if token not in {"qualified", "safe"}]
            module, *rest = tokens
            qualifier = rest[1] if rest[:1] == ["as"] and len(rest) > 1 else module
            names = frozenset(rest[rest.index("(") + 1:]) if "(" in rest else None
            self.imports.append(ImportedNames(module, qualifier, qualified, names, "hiding" in rest))
        declarations = re.findall(r"(?ms)^(?:data|newtype|type)\b.*?(?=^\S|\Z)", self.masked)
        self.local_types = set()
        for declaration in declarations:
            declared = re.match(r"(?:data|newtype|type)\s+([A-Z][\w']*)", declaration)
            if declared:
                self.local_types.add(declared[1])
            self.local_types.update(re.findall(r"[=|]\s*(True|False)\b", declaration))

    def resolves(self, token, name):
        parts = token.rsplit(".", 1)
        if parts[-1] != name:
            return False
        qualifier = parts[0] if len(parts) == 2 else None
        if qualifier is None and name in self.local_types:
            return False
        return any(
            item.module in PROVIDERS[name]
            and (item.qualifier == qualifier if qualifier else not item.qualified)
            and item.contains(name)
            for item in self.imports
        )


def split_outer(tokens, separator):
    parts = []
    start = 0
    depth = 0
    for index, token in enumerate(tokens):
        if token in {"(", "[", "{"}:
            depth += 1
        elif token in {")", "]", "}"}:
            depth -= 1
        elif token == separator and depth == 0:
            parts.append(tokens[start:index])
            start = index + 1
    return [*parts, tokens[start:]]


def strip_parens(tokens):
    while tokens[:1] == ["("] and tokens[-1:] == [")"]:
        depth = 0
        for index, token in enumerate(tokens):
            depth += (token == "(") - (token == ")")
            if depth == 0:
                break
        if index != len(tokens) - 1:
            break
        tokens = tokens[1:-1]
    return tokens


def bad_input(tokens, names):
    tokens = strip_parens(tokens)
    tokens = split_outer(tokens, "=>")[-1]
    if tokens[:1] == ["forall"]:
        tokens = split_outer(tokens, ".")[-1]
    parts = split_outer(tokens, "->")
    if len(parts) == 1:
        return False
    for domain in parts[:-1]:
        domain = strip_parens(domain)
        if domain and names.resolves(domain[0], "LinkedList"):
            argument = strip_parens(domain[1:])
            if len(argument) == 1 and names.resolves(argument[0], "Char"):
                return True
        if bad_input(domain, names):
            return True
    return bad_input(parts[-1], names)


def signature_violations(source, path):
    names = SourceNames(code_source(source, path))
    lines = names.masked.splitlines()
    declaration = r"(?:[a-z_][\w']*|\([!#$%&*+./<=>?@\\^|~:-]+\))"
    pattern = re.compile(r"^(" + declaration + r"(?:\s*,\s*" + declaration + r")*)\s*::([^\n]*)", re.MULTILINE)
    for match in pattern.finditer(names.masked):
        index = names.masked.count("\n", 0, match.start())
        end = names.masked.count("\n", 0, match.end())
        declarations = [name.strip() for name in match[1].split(",")]
        parts = [match[2]]
        for continuation in lines[end + 1:]:
            if continuation.strip() and not continuation[:1].isspace():
                break
            parts.append(continuation)
        signature = " ".join(parts).split(";", 1)[0]
        if bad_input(TOKEN.findall(signature), names):
            for declaration in declarations:
                if (path, names.module, declaration) not in CONVERSIONS:
                    yield declaration, index + 1, signature.strip()


def candidate_rule(idea, names):
    if idea["hint"] == "NeoHaskell candidate: Boolean case":
        arms = re.findall(r"(?m)^  (" + NAME + r")\s*->", idea["from"])
        if len(arms) == 2 and {arm.rsplit(".", 1)[-1] for arm in arms} == {"True", "False"}:
            if all(names.resolves(arm, arm.rsplit(".", 1)[-1]) for arm in arms):
                return "NeoHaskell: use if for Bool"
        return None
    if idea["hint"] == "NeoHaskell candidate: Text conversion":
        quote = re.fullmatch(r"((?:\(\s*)*)\[(" + NAME + r")\|(?:(?!\|\])[\s\S])*\|\]((?:\s*\))*)", idea["to"] or "")
        if quote and quote[1].count("(") == quote[3].count(")") and names.resolves(quote[2], "fmt"):
            return "NeoHaskell: fmt already produces Text"
        return None
    return idea["hint"]
