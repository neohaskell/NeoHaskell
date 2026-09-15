#!/usr/bin/env python3
"""Adapt Codex lifecycle events to the shared repository guards."""
import importlib.util
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]


def module(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / 'scripts/guards' / f'{name}.py')
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def patch_edits(command, root, cwd=None):
    """Read Codex patch sections as old/new fragments; never execute patch text."""
    if not command.startswith('*** Begin Patch\n') or not command.rstrip().endswith('*** End Patch'):
        raise ValueError('Expected a complete apply_patch document')
    cwd = Path(cwd) if cwd else root
    if not cwd.resolve().is_relative_to(root.resolve()):
        raise ValueError("Hook working directory leaves the repository")
    edits = []
    current = None
    for line in command.splitlines()[1:]:
        match = re.fullmatch(r'\*\*\* (Add|Update|Delete) File: (.+)', line)
        if match:
            kind, path = match.groups()
            target = (cwd / path).resolve()
            if not target.is_relative_to(root.resolve()):
                raise ValueError('Patch path leaves the repository')
            current = {'path': str(target.relative_to(root.resolve())), 'old': [], 'new': [], 'dest': None}
            edits.append(current)
            if kind == 'Delete':
                current['old'] = target.read_text().splitlines()
            continue
        if line.startswith('*** Move to: '):
            if current is None:
                raise ValueError('Move without file section')
            target = (cwd / line.removeprefix('*** Move to: ')).resolve()
            if not target.is_relative_to(root.resolve()):
                raise ValueError('Move path leaves the repository')
            current['dest'] = str(target.relative_to(root.resolve()))
        elif current is not None and line[:1] in (' ', '+', '-'):
            if line[0] != '+': current['old'].append(line[1:])
            if line[0] != '-': current['new'].append(line[1:])
    if not edits:
        raise ValueError('Patch contains no file sections')
    return edits


def pre_edit(command, root, cwd=None):
    edits = patch_edits(command, root, cwd)
    branch = subprocess.run(['git', 'branch', '--show-current'], cwd=root,
                            capture_output=True, text=True, check=True).stdout.strip()
    if branch == 'main':
        return ['Cannot edit on main. Create a feature branch first.']
    dialect, expectations = module('dialect-guard'), module('expectation-guard')
    problems = []
    approved = (root / '.agents/allow-expectation-edits').exists()
    for edit in edits:
        paths = [edit['path'], edit['dest'] or edit['path']]
        if '.agents/allow-expectation-edits' in paths:
            problems.append('The expectation approval marker is maintainer-controlled.')
        payload = {'file_path': edit['path'], 'old_string': '\n'.join(edit['old']),
                   'new_string': '\n'.join(edit['new'])}
        problems += [message for _, message, _ in dialect.check('Edit', payload)]
        problems += [f'Existing expectation removed: {line}' for line in
                     expectations.check('Edit', payload, approved)]
        if edit['dest']:
            payload['file_path'] = edit['dest']
            problems += [message for _, message, _ in dialect.check('Edit', payload)]
    return problems


def post_edit(command, root, cwd=None):
    edits = patch_edits(command, root, cwd)
    touched_tests = False
    for edit in edits:
        path = root / (edit['dest'] or edit['path'])
        if path.suffix == '.hs' and path.is_file():
            subprocess.run([str(ROOT / 'scripts/with-toolchain'), 'fourmolu', '--mode', 'inplace', str(path)],
                           cwd=root, check=True, timeout=50, stdout=sys.stderr)
        touched_tests |= path.name.endswith('Spec.hs') or path.suffix == '.hurl'
    return 'Test file modified — run the relevant ./dev test or testbed suite.' if touched_tests else ''


def handle(event, root=ROOT):
    kind = event.get('hook_event_name', '')
    tool = event.get('tool_name', '')
    command = event.get('tool_input', {}).get('command', '')
    if kind == 'SessionStart':
        # Preserve best-effort Postgres warmup; the portable dev commands own Nix/PATH.
        if shutil.which('docker'):
            subprocess.Popen(['docker', 'compose', 'up', '-d'], cwd=root,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             start_new_session=True)
        return 'Read the nearest AGENTS.md. Run ./dev watch before Haskell edits; ./dev check after edits. Cloud provisioning uses scripts/cloud-setup.sh.'
    if kind == 'PreToolUse' and tool == 'apply_patch':
        errors = pre_edit(command, root, event.get('cwd'))
        if errors: raise ValueError('\n'.join(errors))
    if kind == 'PostToolUse' and tool == 'apply_patch':
        return post_edit(command, root, event.get('cwd'))
    if kind == 'PreToolUse' and tool == 'Bash':
        # Retain the former explicit destructive-command denials as early feedback.
        if re.search(r'\bgit\s+(?:push\b[^\n;]*(?:--force\b|\s-f\b)|reset\s+--hard\b)|\brm\s+-[a-z]*r[a-z]*f\b', command):
            raise ValueError('Destructive command blocked by repository policy.')
    return ''


def main():
    try:
        event = json.load(sys.stdin)
        context = handle(event)
        if context:
            print(json.dumps({'hookSpecificOutput': {'hookEventName': event['hook_event_name'],
                                                      'additionalContext': context}}))
        return 0
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        print(f'Codex repository hook: {error}', file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
