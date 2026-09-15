#!/usr/bin/env python3
"""Exercise native Codex event payloads through the shared guard adapter."""
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('adapter', Path(__file__).with_name('codex-hooks.py'))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
config = json.loads((module.ROOT / '.codex/hooks.json').read_text())
assert set(config['hooks']) == {'PreToolUse', 'PostToolUse', 'SessionStart'}
assert 'apply_patch' in config['hooks']['PreToolUse'][0]['matcher']

with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    subprocess.run(['git', 'init', '-b', 'feature', str(root)], check=True, capture_output=True)
    (root / 'core').mkdir()
    def event(text, kind='PreToolUse', cwd=None):
        return {'hook_event_name': kind, 'tool_name': 'apply_patch', 'cwd': str(cwd or root),
                'tool_input': {'command': '*** Begin Patch\n' + text + '\n*** End Patch'}}
    def rejected(item, expected):
        try:
            module.handle(item, root)
        except ValueError as exc:
            assert expected in str(exc), str(exc)
        else:
            raise AssertionError('Expected rejection: ' + expected)
    clean = event('*** Add File: core/Foo.hs\n+value = Task.yield 1')
    assert module.handle(clean, root) == ''
    rejected(event('*** Add File: core/Foo.hs\n+value = pure 1'), 'pure')
    rejected(event('*** Update File: core/TestSpec.hs\n@@\n-result `shouldBe` 1\n+result `shouldBe` 2'), 'Existing expectation')
    rejected(event('*** Add File: .agents/allow-expectation-edits\n+yes'), 'maintainer-controlled')
    rejected(event('*** Add File: ../outside.hs\n+value = 1'), 'leaves the repository')
    rejected(event('*** Add File: Foo.hs\n+value = pure 1', cwd=root / 'core'), 'pure')
    (root / 'core/TestSpec.hs').write_text('result `shouldBe` 1\n')
    rejected(event('*** Delete File: core/TestSpec.hs'), 'Existing expectation')
    (root / '.agents').mkdir()
    (root / '.agents/allow-expectation-edits').write_text('maintainer approved\n')
    assert module.handle(event('*** Update File: core/TestSpec.hs\n@@\n-result `shouldBe` 1\n+result `shouldBe` 2'), root) == ''
    subprocess.run(['git', 'symbolic-ref', 'HEAD', 'refs/heads/main'], cwd=root, check=True)
    rejected(clean, 'Cannot edit on main')
    with patch.object(module.subprocess, 'run') as run:
        result = module.handle(event('*** Update File: core/TestSpec.hs\n@@\n+value = Task.yield 1', kind='PostToolUse'), root)
        assert 'Test file modified' in result
        assert 'fourmolu' in run.call_args.args[0]
    with patch.object(module.shutil, 'which', return_value=None):
        assert './dev watch' in module.handle({'hook_event_name': 'SessionStart'}, root)
    rejected({'hook_event_name': 'PreToolUse', 'tool_name': 'Bash',
              'tool_input': {'command': 'git reset --hard'}}, 'Destructive command')
print('codex-hooks self-test: OK — native patches, nested cwd, branch, dialect, expectations, formatting, and session guidance')
