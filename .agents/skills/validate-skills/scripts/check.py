#!/usr/bin/env python3
import argparse
import json
from pathlib import Path
import re
import sys
from urllib.parse import unquote, urlsplit

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).parent))
from frontmatter import parse_frontmatter

NAME = re.compile(r'^[a-z0-9]+(?:-[a-z0-9]+)*$')
LINK = re.compile(r'\[[^\]]*\]\(([^)]+)\)')
RESOURCE = re.compile(r'\.agents/skills/[a-z0-9-]+/(?:scripts|references|assets|tests)/[A-Za-z0-9_.\-/]+')


def prose(text):
    lines = []
    fence = None
    for line in text.splitlines():
        marker = re.match(r'^\s*(`{3,}|~{3,})', line)
        if marker:
            token = marker[1]
            if fence is None:
                fence = token
            elif token[0] == fence[0] and len(token) >= len(fence):
                fence = None
            continue
        if fence is None:
            lines.append(line)
    return '\n'.join(lines)


def link_errors(path, root):
    text = prose(path.read_text())
    errors = []
    for match in LINK.finditer(text):
        raw_target = match[1].strip()
        if raw_target in {'url', '<...>', '{...}'}:
            continue
        target = raw_target
        if target.startswith('<'):
            target = target[1:].split('>', 1)[0]
        else:
            target = target.split(' "', 1)[0].split(" '", 1)[0]
        if target == '...':
            continue
        parsed = urlsplit(target)
        if parsed.scheme or parsed.netloc or not parsed.path:
            continue
        destination = path.parent / unquote(parsed.path)
        if not destination.exists():
            errors.append(f'{path}: missing link {target}')
    for target in RESOURCE.findall(text):
        if not (root / target).exists():
            errors.append(f'{path}: missing resource {target}')
    return errors


def validate(root, skills_root=None, skill=None):
    root = root.resolve()
    collection = skills_root.resolve() if skills_root else root / '.agents/skills'
    files = [skill.resolve() / 'SKILL.md'] if skill else sorted(collection.glob('*/SKILL.md'))
    errors = []
    names = {}
    if not files:
        return {'skills': 0, 'documents': 0, 'errors': ['No skill entrypoints found']}
    for path in files:
        if not path.is_file():
            errors.append(f'Missing entrypoint {path}')
            continue
        fields, problems = parse_frontmatter(path.read_text())
        errors.extend(f'{path}: {problem}' for problem in problems)
        fields = fields or {}
        name = fields.get('name')
        description = fields.get('description')
        if not isinstance(name, str) or not NAME.fullmatch(name) or len(name) > 64:
            errors.append(f'{path}: invalid name')
        elif name != path.parent.name:
            errors.append(f'{path}: name differs from folder')
        if not isinstance(description, str) or not description.strip():
            errors.append(f'{path}: description is empty or invalid')
        if isinstance(name, str):
            if name in names:
                errors.append(f'{path}: duplicate name {name}, also in {names[name]}')
            names[name] = str(path)
    documents = sorted({path for entry in files if entry.is_file() for path in entry.parent.rglob('*.md')})
    is_project_collection = not skill and not skills_root
    if is_project_collection:
        documents += [path for path in [root / 'AGENTS.md', *sorted((root / '.agents/workflow').rglob('*.md'))] if path.is_file()]
        for directory in ['.agents/workflow/scripts', '.agents/workflow/references', '.agents/agents']:
            if any(path.is_file() for path in (root / directory).rglob('*')):
                errors.append(f'{directory}: move skill resources into their owner')
    for path in documents:
        try:
            errors.extend(link_errors(path, root))
        except (OSError, ValueError) as error:
            errors.append(f'{path}: {error}')
    return {'skills': len(files), 'documents': len(documents), 'errors': errors}


def main():
    parser = argparse.ArgumentParser(description='Validate required skill metadata and owned resource links.')
    parser.add_argument('--repo', type=Path, default=Path.cwd())
    parser.add_argument('--skills-root', type=Path)
    parser.add_argument('--skill', type=Path)
    args = parser.parse_args()
    result = validate(args.repo, args.skills_root, args.skill)
    print(json.dumps(result, indent=2))
    return 1 if result['errors'] else 0


if __name__ == '__main__':
    raise SystemExit(main())
