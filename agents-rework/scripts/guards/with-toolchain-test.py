#!/usr/bin/env python3
"""Exercise fresh-session Nix discovery without installing Nix or entering a flake."""
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
PROFILE = '/nix/var/nix/profiles/default/bin'

with tempfile.TemporaryDirectory() as directory:
    root = Path(directory)
    scripts = root / 'scripts'
    scripts.mkdir()
    profile = root / 'system profile/bin'
    profile.mkdir(parents=True)
    # Relocate only the installation path so the actual wrapper can run against
    # a disposable filesystem, without relying on the developer's installed Nix.
    wrapper = scripts / 'with-toolchain'
    wrapper.write_text((ROOT / 'scripts/with-toolchain').read_text().replace(PROFILE, str(profile)))
    bare = root / 'bare'
    bare.mkdir()
    for command in ('bash', 'dirname'):
        (bare / command).symlink_to(shutil.which(command))
    env = {'PATH': str(bare), 'HOME': str(root)}

    def executable(path, body):
        path.write_text('#!/usr/bin/env bash\n' + body)
        path.chmod(0o755)

    def run(*args):
        return subprocess.run([str(bare / 'bash'), str(wrapper), *args],
                              env=env, capture_output=True, text=True)

    executable(bare / 'host-tool', 'printf "host:%s\\n" "$@"\nexit 23\n')
    executable(profile / 'nix', 'printf "profile:%s\\n" "$@"\nexit 37\n')
    result = run('host-tool', 'argument with spaces', '')
    assert result.returncode == 37, result
    assert result.stdout.splitlines() == ['profile:develop', 'profile:--command',
                                          'profile:host-tool', 'profile:argument with spaces', 'profile:']
    assert not result.stderr, result.stderr

    executable(bare / 'nix', 'printf "path:%s\\n" "$@"\n')
    result = run('host-tool')
    assert result.returncode == 0 and result.stdout.startswith('path:develop\n'), result
    (bare / 'nix').unlink()
    (profile / 'nix').unlink()
    result = run('host-tool', 'argument with spaces')
    assert result.returncode == 23 and result.stdout == 'host:argument with spaces\n', result
    assert 'version-drift risk' in result.stderr, result
    assert run().returncode == 2

print('with-toolchain self-test: OK — fresh PATH, explicit Nix precedence, arguments, exit codes, and host fallback')
