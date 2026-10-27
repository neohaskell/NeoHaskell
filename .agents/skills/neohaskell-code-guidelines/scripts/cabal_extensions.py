"""Read unconditional Cabal component defaults for the HLint parser."""

from dataclasses import dataclass
from pathlib import Path
import re
import shlex

from source_rules import code_source, source_parts


FIELDS = {"import", "default-extensions", "default-language", "hs-source-dirs", "ghc-options"}
TEMPLATE = Path("neo/assets/templates/project.cabal.j2")


@dataclass(frozen=True)
class Component:
    owner: Path
    directories: tuple
    extensions: tuple


def extension_profile(values):
    settings = {}
    language = None
    for value in values:
        if value in {"Haskell98", "Haskell2010", "GHC2021", "GHC2024"}:
            language = value
        else:
            if not re.fullmatch(r"[A-Z][A-Za-z0-9]*", value):
                raise ValueError(f"Unsupported Cabal language extension {value!r}.")
            key = value[2:] if value.startswith("No") and value[2:3].isupper() else value
            settings.pop(key, None)
            settings[key] = value
    return tuple(([language] if language else []) + list(settings.values()))


def read_components(root, path, package_root):
    stanzas = {}
    current = None
    field = None
    for number, raw in enumerate((root / path).read_text().splitlines(), 1):
        line = raw.split("--", 1)[0].rstrip()
        if not line.strip():
            continue
        if path == TEMPLATE and line.lstrip().startswith("{%"):
            if line.strip() not in {"{% for dep, ver in dependencies %}", "{% endfor %}", "{% for module in modules %}", "{% if not is_library %}", "{% endif %}", "{% if test_modules %}", "{% for module in test_modules %}"} and not line.startswith("{% if description %}"):
                raise ValueError(f"Unsupported template condition in {path}:{number}.")
            continue
        if re.match(r"\s*(if\b|elif\b|else\b)", line):
            raise ValueError(f"Conditional Cabal settings need explicit support in {path}:{number}.")
        if not line[:1].isspace():
            match = re.fullmatch(r"(common|library|executable|test-suite|benchmark)(?:\s+(.+))?", line)
            current = (match[1], match[2] or "") if match else None
            field = None
            if current:
                if current in stanzas:
                    raise ValueError(f"Duplicate Cabal stanza in {path}:{number}.")
                stanzas[current] = []
            continue
        if current is None:
            continue
        match = re.match(r"  ([\w-]+):\s*(.*)", line)
        if match:
            field, value = match.groups()
            stanzas[current].append((field, value))
        elif field:
            previous, value = stanzas[current][-1]
            stanzas[current][-1] = (previous, value + " " + line.strip())
        else:
            raise ValueError(f"Unsupported Cabal stanza syntax in {path}:{number}.")

    def expand(key, visiting=()):
        if key in visiting or key not in stanzas:
            raise ValueError(f"Invalid common stanza import {key} in {path}.")
        fields = []
        for field, value in stanzas[key]:
            if field == "import":
                for name in re.split(r"[\s,]+", value.strip()):
                    fields.extend(expand(("common", name), (*visiting, key)))
            else:
                fields.append((field, value))
        return fields

    components = []
    for key in stanzas:
        if key[0] == "common":
            continue
        languages = []
        extensions = []
        options = []
        directories = []
        for field, value in expand(key):
            if field not in FIELDS:
                continue
            if "{{" in value or "{%" in value:
                raise ValueError(f"Templated {field} in {path}.")
            if field == "default-language":
                languages.extend(value.split())
            elif field == "default-extensions":
                extensions.extend(re.split(r"[\s,]+", value.strip()) if value.strip() else [])
            elif field == "ghc-options":
                options.extend(option[2:] for option in shlex.split(value) if option.startswith("-X"))
            elif field == "hs-source-dirs":
                directories.extend(shlex.split(value.replace(",", " ")))
        resolved = tuple((root / package_root / directory).resolve().relative_to(root.resolve()) for directory in directories or ["."])
        components.append(Component(path, resolved, extension_profile([*languages, *extensions, *options])))
    return components


def profiles(root, paths, tracked):
    components = []
    for path in tracked:
        if path.suffix == ".cabal":
            components.extend(read_components(root, path, path.parent))
    if TEMPLATE in tracked:
        components.extend(read_components(root, TEMPLATE, Path("neo/starter")))
    result = {}
    for path in paths:
        owners = [component for component in components if any(path.is_relative_to(directory) for directory in component.directories)]
        settings = {component.extensions for component in owners}
        if len(settings) > 1:
            raise ValueError(f"Conflicting component language settings for {path}.")
        if components and not owners:
            raise ValueError(f"No Cabal component language settings for {path}.")
        result[path] = next(iter(settings)) if settings else ()
    return result


def parser_source(source, path, extensions):
    _, pragmas = source_parts(code_source(source, path))
    strict = "Strict" in extensions
    replacements = []
    for start, end, pragma in pragmas:
        if re.match(r"\{-#\s*LINE\b", pragma):
            raise ValueError(f"Source LINE pragmas need explicit diagnostic mapping support in {path}.")
        language = re.fullmatch(r"\{-#\s*LANGUAGE\s+([\s\S]*?)#-\}", pragma)
        options = re.fullmatch(r"\{-#\s*OPTIONS_GHC\s+([\s\S]*?)#-\}", pragma)
        if options and any(option.startswith("-X") for option in shlex.split(options[1])):
            raise ValueError(f"Source OPTIONS_GHC language flags need explicit support in {path}.")
        if not language:
            continue
        values = [value for value in re.split(r"[\s,]+", language[1].strip()) if value]
        for value in values:
            if value in {"Strict", "NoStrict"}:
                strict = value == "Strict"
        kept = [value for value in values if value not in {"Strict", "NoStrict"}]
        if kept != values:
            replacement = "{-# LANGUAGE " + ", ".join(kept) + " #-}" if kept else ""
            replacement += "\n" * pragma.count("\n")
            replacements.append((start, end, replacement))
    if path.suffix == ".lhs":
        if extensions or replacements:
            raise ValueError(f"Literate Haskell language defaults need explicit support in {path}.")
        return source
    for start, end, replacement in reversed(replacements):
        source = source[:start] + replacement + source[end:]
    escaped = path.as_posix().replace("\\", "\\\\").replace('"', '\\"')
    prefix = "{-# LANGUAGE Strict #-}\n" if strict else ("{-# LANGUAGE NoStrict #-}\n" if replacements or "Strict" in extensions else "")
    return prefix + f'{{-# LINE 1 "{escaped}" #-}}\n' + source
