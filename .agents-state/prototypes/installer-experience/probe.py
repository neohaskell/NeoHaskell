import errno
import fcntl
import os
from pathlib import Path
import pty
import select
import signal
import struct
import subprocess
import sys
import termios
import time


ROOT = Path(__file__).resolve().parent
SCRIPT = str(ROOT / 'prototype.py')
EVIDENCE = ROOT / 'captures'
EVIDENCE.mkdir(exist_ok=True)
ENV = dict(os.environ, TERM='xterm-256color', PYTHONDONTWRITEBYTECODE='1')
ENV.pop('CI', None)
ENV.pop('NO_COLOR', None)


def controlling_terminal():
    fcntl.ioctl(0, termios.TIOCSCTTY, 0)


def session(name, args, actions, width=80):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, width, 0, 0))
    proc = subprocess.Popen([sys.executable, SCRIPT] + args, stdin=slave, stdout=slave,
                            stderr=slave, env=ENV, start_new_session=True,
                            preexec_fn=controlling_terminal)
    os.close(slave)
    data = bytearray()
    offset = 0
    try:
        for pattern, action in actions:
            deadline = time.monotonic() + 15
            while pattern.encode() not in data[offset:]:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise AssertionError(name + ' did not display ' + pattern)
                ready, _, _ = select.select([master], [], [], remaining)
                if not ready:
                    raise AssertionError(name + ' timed out')
                try:
                    chunk = os.read(master, 65536)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    chunk = b''
                assert chunk, name + ' exited before ' + pattern
                data.extend(chunk)
            offset = len(data)
            if action is not None:
                os.write(master, action)
        deadline = time.monotonic() + 3
        while True:
            remaining = deadline - time.monotonic()
            assert remaining > 0, name + ' did not close output'
            ready, _, _ = select.select([master], [], [], remaining)
            assert ready, name + ' did not finish'
            try:
                chunk = os.read(master, 65536)
            except OSError as error:
                if error.errno != errno.EIO:
                    raise
                chunk = b''
            if not chunk:
                break
            data.extend(chunk)
        proc.wait(timeout=2)
        return bytes(data), proc.returncode
    finally:
        EVIDENCE.joinpath(name + '.ansi').write_bytes(data)
        try:
            os.killpg(proc.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, signal.SIGKILL)
            proc.wait(timeout=2)
        os.close(master)


for variant in ('1', '2', '3'):
    output, code = session('variant-' + variant, ['--variant', variant], [
        ('Enter preview', b'\r'),
        ('Continue', b'd'),
        ('Never expand trusted-users', b'\r'),
        ("You're ready to build.", b'q'),
    ])
    assert code == 0
    assert b'neo and neoctl are available' in output

output, code = session('network-retry', ['--scenario', 'network', '--variant', '3'], [
    ('Enter preview', b'\r'),
    ('Continue', b'\r'),
    ('R retry simulation', b'r'),
    ("You're ready to build.", b'q'),
])
assert code == 0
assert output.index(b'Setup is not complete') < output.index(b"You're ready to build.")

output, code = session('interrupt', [], [
    ('Enter preview', b'\r'),
    ('Continue', b'\r'),
    ('Prepare Nix', b'\x03'),
    ('Installation cancelled.', None),
])
assert code == 130
assert b"You're ready to build." not in output

output, code = session('narrow-panel', ['--variant', '3', '--yes'], [
    ('Simulated result.', None),
], width=42)
assert code == 0

for scenario in ('new', 'existing', 'network', 'systemd', 'managed'):
    result = subprocess.run([sys.executable, SCRIPT, '--scenario', scenario, '--yes'],
                            env=ENV, capture_output=True, check=False)
    EVIDENCE.joinpath(scenario + '-plain.txt').write_bytes(result.stdout)
    assert b'\x1b' not in result.stdout
    assert (result.returncode == 0) == (scenario in ('new', 'existing'))
    assert (b'NeoHaskell is ready.' in result.stdout) == (result.returncode == 0)

result = subprocess.run([sys.executable, SCRIPT], input=b'', capture_output=True, env=ENV)
assert result.returncode == 2
assert b'--yes' in result.stdout
print('PASS. Three PTY flows, details, failure and retry, Ctrl-C, narrow terminal, five plain scenarios, missing consent.')
