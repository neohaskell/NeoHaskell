#!/usr/bin/env python3
import argparse
from pathlib import Path
import re
import subprocess
import sys

sys.dont_write_bytecode = True

LIFECYCLE = ('Accepted', 'Proposed', 'Deprecated', 'Superseded', 'Rejected', 'Historical')


ALIASES = {'implemented': 'Accepted', 'done': 'Accepted'}


FILE_RE = re.compile('^(\\d{4})-.*\\.md$')


ROW_RE = re.compile('^\\|\\s*\\[(\\d{4})\\]\\(([^)]+)\\)\\s*\\|.*\\|\\s*([^|]+?)\\s*\\|\\s*$')


def lifecycle_of(text):
    for word in re.findall('[A-Za-z]+', text):
        lower = word.lower()
        for kw in LIFECYCLE:
            if lower == kw.lower():
                return kw
        if lower in ALIASES:
            return ALIASES[lower]
    return None


def file_status(path):
    lines = path.read_text(encoding='utf-8').splitlines()
    for (i, line) in enumerate(lines):
        if re.match('^#+\\s*Status\\b', line.strip(), re.IGNORECASE):
            for follow in lines[i + 1:]:
                if follow.strip():
                    return lifecycle_of(follow)
            break
    return None


def collect_files(decisions_dir):
    files = {}
    errors = []
    for p in sorted(decisions_dir.glob('*.md')):
        m = FILE_RE.match(p.name)
        if not m:
            continue
        num = m.group(1)
        if num in files:
            errors.append(f"ADR number {num} is used by two files: '{files[num]}' and '{p.name}' — renumber one (NNNN must be unique)")
            continue
        files[num] = p.name
    return (files, errors)


def check(decisions_dir, index_path):
    (files, errors) = collect_files(decisions_dir)
    rows = {}
    seen = []
    for line in index_path.read_text(encoding='utf-8').splitlines():
        m = ROW_RE.match(line)
        if not m:
            continue
        (num, link, status) = (m.group(1), m.group(2).strip(), m.group(3).strip())
        seen.append(num)
        rows[num] = (link, status)
    dupes = sorted({n for n in seen if seen.count(n) > 1})
    for n in dupes:
        errors.append(f'ADR {n} is indexed more than once')
    for (num, name) in sorted(files.items()):
        if num not in rows:
            errors.append(f'ADR {num} ({name}) exists on disk but is missing from the index')
            continue
        (link, status_cell) = rows[num]
        if link != name:
            errors.append(f"ADR {num} index link points to '{link}', but the file is '{name}'")
        want = file_status(decisions_dir / name)
        got = lifecycle_of(status_cell)
        if want is None:
            errors.append(f'ADR {num} ({name}) has no parseable `## Status` section')
        elif got != want:
            errors.append(f"ADR {num} status mismatch: index says '{status_cell}' (→ {got}) but {name} declares '{want}'")
    for num in sorted(rows):
        if num not in files:
            errors.append(f'index lists ADR {num} but no matching file exists in docs/decisions/')
    return (errors, len(files))


def main():
    parser = argparse.ArgumentParser(description='Check an ADR index against its files and declared statuses.')
    parser.add_argument('--directory', type=Path)
    args = parser.parse_args()
    if args.directory:
        directory = args.directory.resolve()
    else:
        root = Path(subprocess.check_output(['git', '-C', str(Path(__file__).parent), 'rev-parse', '--show-toplevel'], text=True).strip())
        directory = root / 'docs/decisions'
    try:
        errors, count = check(directory, directory / 'README.md')
    except OSError as error:
        errors, count = [str(error)], 0
    for error in errors:
        print(error, file=sys.stderr)
    print(f'{count} ADRs checked; {len(errors)} errors')
    return 1 if errors else 0


if __name__ == '__main__':
    raise SystemExit(main())
