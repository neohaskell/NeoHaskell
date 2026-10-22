#!/usr/bin/env python3
"""Find marker-owned defaults repeated in the embedded starter or testbed.

This is a bounded source checker, not a Haskell parser. It reads ordinary
top-level data declarations, attached deriving clauses, empty instance
declarations, and calls that pass an unqualified local type to a known service
marker. It ignores comments and string literals. Template Haskell source
shapes that define marker ownership are checked explicitly and fail closed if
they change, so this inventory cannot quietly drift from the generators.

Standalone deriving declarations, generated declarations, qualified marker
targets, and marker calls that pass a computed name are outside the checked
syntax. Keep the consumer contract responsible for compiler behavior beyond
this source-level check.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

SCRIPT = Path(__file__).resolve()
ROOT = next(
    (candidate for candidate in SCRIPT.parents if (candidate / "flake.nix").is_file()),
    None,
)


class FreshnessError(RuntimeError):
    pass


@dataclass(frozen=True)
class Finding:
    path: str
    line: int
    type_name: str
    class_name: str
    marker: str
    declaration: str


def _mask_haskell(source: str, *, strings: bool = True) -> str:
    output = list(source)
    index = 0
    block_depth = 0
    in_line_comment = False
    in_string = False
    escaped = False

    def blank(position: int) -> None:
        if output[position] != "\n":
            output[position] = " "

    while index < len(source):
        pair = source[index : index + 2]
        char = source[index]

        if in_line_comment:
            if char == "\n":
                in_line_comment = False
            else:
                blank(index)
            index += 1
            continue

        if block_depth:
            if pair == "{-":
                block_depth += 1
                blank(index)
                blank(index + 1)
                index += 2
            elif pair == "-}":
                block_depth -= 1
                blank(index)
                blank(index + 1)
                index += 2
            else:
                blank(index)
                index += 1
            continue

        if strings and in_string:
            blank(index)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            index += 1
            continue

        if pair == "{-":
            block_depth = 1
            blank(index)
            blank(index + 1)
            index += 2
            continue

        if pair == "--":
            following = source[index + 2 : index + 3]
            symbol_chars = "!#$%&*+./<=>?@\\^|-~:"
            if not following or following not in symbol_chars:
                in_line_comment = True
                blank(index)
                blank(index + 1)
                index += 2
                continue

        if strings and char == '"':
            in_string = True
            blank(index)
            index += 1
            continue

        index += 1

    return "".join(output)


def _class_name(name: str) -> str:
    return name.split(".")[-1]


def _function_body(source: str, signature_name: str, definition: str) -> str:
    code = _mask_haskell(source, strings=False)
    match = re.search(
        rf"(?m)^{re.escape(definition)}\s*::[^\n]*\n{re.escape(definition)}\b[^=\n]*=",
        code,
    )
    if match is None:
        raise FreshnessError(
            f"cannot locate the supported TH definition for {signature_name}"
        )
    remainder = code[match.end() :]
    end_positions = [match.start() for match in re.finditer(r"(?m)^\{-#\s*INLINE\b", remainder)]
    next_signature = re.search(r"(?m)^[A-Za-z_][A-Za-z0-9_']*\s*::", remainder)
    if next_signature is not None:
        end_positions.append(next_signature.start())
    return remainder[: min(end_positions)] if end_positions else remainder


def _quoted_helper_classes(source: str, helper_name: str, target_name: str) -> set[str]:
    body = _function_body(source, helper_name, helper_name)
    calls = re.findall(
        rf"\bemit(?:StockDeriving|EmptyInstance)\s+''([A-Z][A-Za-z0-9_.]*)\s+{re.escape(target_name)}\b",
        body,
    )
    return {_class_name(name) for name in calls}


def marker_defaults(root: Path) -> dict[str, frozenset[str]]:
    boilerplate_path = root / "core/service/Service/TH/Boilerplate.hs"
    boilerplate = boilerplate_path.read_text(encoding="utf-8")
    shared = _quoted_helper_classes(
        boilerplate, "emitJsonAndDerivingBoilerplate", "typeName"
    )
    if not shared:
        raise FreshnessError(
            "no class defaults were found in Service.TH.Boilerplate; update this checker for the new generator shape"
        )

    event_source = (root / "core/service/Service/Event/TH.hs").read_text(encoding="utf-8")
    event_code = _mask_haskell(event_source, strings=False)
    if (
        re.search(r"\bderiveEvent\s*=\s*emitJsonAndDerivingBoilerplate\b", event_code)
        is None
        or re.search(r"\bevent\s*=\s*deriveEvent\b", event_code) is None
    ):
        raise FreshnessError("unsupported deriveEvent generator shape; update this checker")

    command_source = (root / "core/service/Service/CommandExecutor/TH.hs").read_text(
        encoding="utf-8"
    )
    command_code = _mask_haskell(command_source, strings=False)
    command_body = _function_body(command_source, "command", "command")
    if re.search(r"\bderiveCommand\s*=\s*command\b", command_code) is None:
        raise FreshnessError("unsupported deriveCommand generator shape; update this checker")
    if re.search(
        r"\bemitJsonAndDerivingBoilerplate\s+someName\b", command_body
    ) is None:
        raise FreshnessError("command no longer calls the shared default generator")
    if re.search(
        r"TH\.InstanceD\s+Nothing\s+\[\]\s+\(TH\.ConT\s+toSchemaClassName\s+`TH\.AppT`\s+TH\.ConT\s+someName\)\s+\[\]",
        command_body,
    ) is None or re.search(r'TH\.lookupTypeName\s+"ToSchema"', command_code) is None:
        raise FreshnessError("unsupported command ToSchema generator shape; update this checker")

    query_source = (root / "core/service/Service/Query/TH.hs").read_text(encoding="utf-8")
    query_code = _mask_haskell(query_source, strings=False)
    query_body = _function_body(query_source, "deriveQuery", "deriveQuery")
    if re.search(
        r"\bemitJsonAndDerivingBoilerplate\s+queryTypeName\b", query_body
    ) is None:
        raise FreshnessError("deriveQuery no longer calls the shared default generator")
    if re.search(
        r"\bemitEmptyInstance\s+toSchemaClassName\s+queryTypeName\b", query_body
    ) is None or re.search(r'lookupOrFail\s+"ToSchema"', query_code) is None:
        raise FreshnessError("unsupported query ToSchema generator shape; update this checker")

    entity_source = (root / "core/service/Service/Entity/TH.hs").read_text(encoding="utf-8")
    entity_body = _function_body(entity_source, "deriveEntity", "deriveEntity")
    entity_classes = _quoted_helper_classes_for_body(entity_body, "entityName")
    if not entity_classes:
        raise FreshnessError("no entity defaults were found; update this checker")

    return {
        "event": frozenset(shared),
        "deriveEvent": frozenset(shared),
        "command": frozenset(shared | {"ToSchema"}),
        "deriveCommand": frozenset(shared | {"ToSchema"}),
        "deriveQuery": frozenset(shared | {"ToSchema"}),
        "deriveEntity": frozenset(entity_classes),
    }


def _quoted_helper_classes_for_body(body: str, target_name: str) -> set[str]:
    calls = re.findall(
        rf"\bemit(?:StockDeriving|EmptyInstance)\s+''([A-Z][A-Za-z0-9_.]*)\s+{re.escape(target_name)}\b",
        body,
    )
    return {_class_name(name) for name in calls}


def _imports(source: str) -> dict[str, str]:
    code = _mask_haskell(source, strings=False)
    aliases: dict[str, str] = {}
    import_pattern = re.compile(
        r"(?m)^\s*import\s+(?:safe\s+)?(?:qualified\s+)?"
        r"(?P<module>[A-Z][A-Za-z0-9_.]*)\b"
        r"(?:\s+qualified)?(?:\s+as\s+(?P<alias>[A-Z][A-Za-z0-9_.]*))?"
    )
    for match in import_pattern.finditer(code):
        module = match.group("module")
        aliases[match.group("alias") or module] = module
        aliases[module] = module
    return aliases


def _canonical_class(
    spelling: str, aliases: dict[str, str], marker_classes: set[str]
) -> str | None:
    parts = spelling.split(".")
    base = parts[-1]
    if base not in marker_classes:
        return None
    if len(parts) == 1:
        return base

    module_prefix = ".".join(parts[:-1])
    module = aliases.get(module_prefix, module_prefix)
    known_modules = {
        "FromJSON": {"Json", "Data.Aeson", "Core"},
        "ToJSON": {"Json", "Data.Aeson", "Core"},
        "Generic": {"GHC.Generics", "Core"},
        "Show": {"GHC.Show", "Prelude", "Core"},
        "ToSchema": {"Schema", "Data.OpenApi", "OpenApi", "Core"},
    }
    if module in known_modules.get(base, set()):
        return base
    return None


def _line(source: str, offset: int) -> int:
    return source.count("\n", 0, offset) + 1


def _top_level_end(source: str, offset: int) -> int:
    line_end = source.find("\n", offset)
    if line_end < 0:
        return len(source)
    next_declaration = re.search(r"(?m)^[^\s\n].*$", source[line_end + 1 :])
    if next_declaration is None:
        return len(source)
    return line_end + 1 + next_declaration.start()


def _deriving_classes(chunk: str) -> list[tuple[int, str]]:
    results: list[tuple[int, str]] = []
    pattern = re.compile(
        r"\bderiving\s*(?:(?:stock|newtype|anyclass)\s*)?\(([^)]*)\)"
        r"|\bderiving\s+(?:(?:stock|newtype|anyclass)\s+)?([A-Z][A-Za-z0-9_.]*)"
    )
    for match in pattern.finditer(chunk):
        classes = match.group(1)
        if classes is None:
            results.append((match.start(2), match.group(2)))
            continue
        for class_match in re.finditer(r"[A-Z][A-Za-z0-9_.]*", classes):
            results.append(
                (match.start(1) + class_match.start(), class_match.group(0))
            )
    return results


def _type_derivings(source: str, aliases: dict[str, str], classes: set[str]) -> dict[str, list[tuple[int, str]]]:
    code = _mask_haskell(source)
    result: dict[str, list[tuple[int, str]]] = {}
    starts = list(re.finditer(r"(?m)^[ \t]*(?:data|newtype)\s+([A-Z][A-Za-z0-9_']*)\b", code))
    for match in starts:
        type_name = match.group(1)
        chunk_end = _top_level_end(code, match.start())
        chunk = code[match.start() : chunk_end]
        for offset, spelling in _deriving_classes(chunk):
            class_name = _canonical_class(spelling, aliases, classes)
            if class_name is not None:
                result.setdefault(type_name, []).append((match.start() + offset, class_name))
    return result


def _empty_instances(source: str, aliases: dict[str, str], classes: set[str]) -> dict[str, list[tuple[int, str]]]:
    code = _mask_haskell(source)
    result: dict[str, list[tuple[int, str]]] = {}
    starts = list(re.finditer(r"(?m)^instance\b", code))
    for match in starts:
        chunk_end = _top_level_end(code, match.start())
        chunk = code[match.start() : chunk_end]
        where = re.search(r"\bwhere\b", chunk)
        if where is not None:
            body = chunk[where.end() :].strip()
            if body and re.fullmatch(r"\{\s*\}", body) is None:
                continue
            header = chunk[: where.start()]
        else:
            header = chunk
        instance_header = header.removeprefix("instance").strip()
        head = instance_header.rsplit("=>", maxsplit=1)[-1].strip()
        instance_head = re.match(
            r"(?P<class>[A-Z][A-Za-z0-9_.]*)\s+(?:\(\s*)?(?P<type>[A-Z][A-Za-z0-9_.']*)\b",
            head,
        )
        if instance_head is None:
            continue
        class_name = _canonical_class(instance_head.group("class"), aliases, classes)
        if class_name is not None:
            result.setdefault(instance_head.group("type").split(".")[-1], []).append(
                (match.start() + instance_head.start("class"), class_name)
            )
    return result


def scan_source(
    path: str, source: str, defaults: dict[str, frozenset[str]]
) -> list[Finding]:
    code = _mask_haskell(source)
    aliases = _imports(source)
    marker_calls = re.compile(
        r"(?:(?:[A-Z][A-Za-z0-9_']*)\.)*"
        r"(?P<marker>deriveCommand|command|deriveEvent|event|deriveQuery|deriveEntity)"
        r"\s+''(?P<type>[A-Z][A-Za-z0-9_']*)"
    )
    targets: dict[str, set[str]] = {}
    for match in marker_calls.finditer(code):
        targets.setdefault(match.group("type"), set()).add(match.group("marker"))

    if not targets:
        return []

    marker_classes = set().union(*(defaults[marker] for markers in targets.values() for marker in markers))
    declarations = _type_derivings(source, aliases, marker_classes)
    declarations_by_type = _empty_instances(source, aliases, marker_classes)
    findings: dict[tuple[str, str, str], Finding] = {}

    for type_name, markers in targets.items():
        for marker in markers:
            for offset, class_name in declarations.get(type_name, []):
                if class_name in defaults[marker]:
                    finding = Finding(
                        path,
                        _line(source, offset),
                        type_name,
                        class_name,
                        marker,
                        "deriving clause",
                    )
                    findings[(type_name, class_name, finding.declaration)] = finding
            for offset, class_name in declarations_by_type.get(type_name, []):
                if class_name in defaults[marker]:
                    finding = Finding(
                        path,
                        _line(source, offset),
                        type_name,
                        class_name,
                        marker,
                        "empty instance",
                    )
                    findings[(type_name, class_name, finding.declaration)] = finding

    return sorted(findings.values(), key=lambda item: (item.line, item.class_name))


def check(root: Path) -> list[Finding]:
    defaults = marker_defaults(root)
    findings: list[Finding] = []
    for directory in (root / "neo/starter", root / "testbed"):
        for path in sorted(directory.rglob("*.hs")):
            findings.extend(
                scan_source(
                    path.relative_to(root).as_posix(),
                    path.read_text(encoding="utf-8"),
                    defaults,
                )
            )
    return findings


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, help="NeoHaskell repository root")
    arguments = parser.parse_args(argv)
    root = arguments.repo.resolve() if arguments.repo else ROOT
    if root is None:
        print("check-consumer-freshness: could not locate repository root", file=sys.stderr)
        return 2
    try:
        findings = check(root)
    except (FreshnessError, OSError) as error:
        print(f"check-consumer-freshness: {error}", file=sys.stderr)
        return 2
    for finding in findings:
        article = "an" if finding.declaration == "empty instance" else "a"
        print(
            f"{finding.path}:{finding.line}: {finding.type_name} repeats marker-owned "
            f"{finding.class_name} in {article} {finding.declaration} ({finding.marker})"
        )
    if findings:
        print(f"check-consumer-freshness: {len(findings)} redundant defaults found", file=sys.stderr)
        return 1
    print("check-consumer-freshness: starter and testbed marker defaults are fresh")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
