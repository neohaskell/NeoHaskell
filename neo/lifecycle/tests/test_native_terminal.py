import argparse
import errno
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import signal
import struct
import sys
import subprocess
import tempfile
import termios
import time


def drive(command, home, mode=None, action=None, piped=False):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 30, 100, 0, 0))
    original = termios.tcgetattr(slave)
    env = dict(os.environ, HOME=str(home), TERM='xterm-256color', SHELL='/bin/zsh')
    env.pop('CI', None)
    env.pop('XDG_STATE_HOME', None)
    if mode is not None:
        env.update(NEO_DASHBOARD_FIXTURE=str(home), NEO_DASHBOARD_MODE=mode)
    def session():
        os.setsid()
        fcntl.ioctl(1, termios.TIOCSCTTY, 0)
    terminal_result = home / 'terminal-settings.txt'
    wrapper = "import pathlib,subprocess,sys,termios; result=subprocess.run(sys.argv[2:],pass_fds=(int(__import__('os').environ['NEO_CLARITY_READY_FD']),)); pathlib.Path(sys.argv[1]).write_text(repr(termios.tcgetattr(1))); sys.exit(result.returncode)"
    ready_read, ready_write = os.pipe()
    env['NEO_CLARITY_READY_FD'] = str(ready_write)
    process = subprocess.Popen([sys.executable, '-c', wrapper, str(terminal_result), *command], stdin=subprocess.PIPE if piped else slave,
                               stdout=slave, stderr=slave, env=env, preexec_fn=session, pass_fds=(ready_write,))
    os.close(ready_write)
    data = bytearray()
    sent = False
    deadline = time.monotonic() + 30
    try:
        while time.monotonic() < deadline:
            ready = select.select([master, ready_read] if action and not sent else [master], [], [], 0.1)[0]
            if ready_read in ready:
                assert os.read(ready_read, 1) == b'R', 'fixture readiness closed'
                os.write(master, action)
                sent = True
            if master in ready:
                try:
                    chunk = os.read(master, 65536)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    break
                if not chunk:
                    break
                data.extend(chunk)
            if process.poll() is not None and not ready:
                break
        else:
            raise AssertionError('installer did not finish within 30 seconds\n' + bytes(data).decode(errors='replace'))
        code = process.wait(timeout=2)
        assert terminal_result.read_text() == repr(original), 'terminal settings were not restored'
        if action:
            assert sent, bytes(data).decode(errors='replace')
        return code, bytes(data)
    finally:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait(timeout=2)
        if process.stdin:
            process.stdin.close()
        os.close(ready_read)
        os.close(master)
        os.close(slave)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--installer', type=Path)
    parser.add_argument('--test-binary', type=Path)
    parser.add_argument('--cargo-artifacts', type=Path)
    args = parser.parse_args()
    if args.cargo_artifacts:
        artifacts = [json.loads(line) for line in args.cargo_artifacts.read_text().splitlines() if line.startswith('{')]
        for artifact in artifacts:
            if artifact.get('reason') != 'compiler-artifact' or not artifact.get('executable'):
                continue
            target = artifact['target']
            if target['name'] == 'neo_lifecycle' and artifact['profile']['test']:
                args.test_binary = Path(artifact['executable'])
            if target['name'] == 'neo-installer' and not artifact['profile']['test']:
                args.installer = Path(artifact['executable'])
    if not args.installer or not args.test_binary:
        parser.error('provide current Cargo artifacts or both binary paths')
    installer = str(args.installer.resolve())
    fixture = [str(args.test_binary.resolve()), '--exact',
               'installation::dashboard::tests::terminal_owner_fixture', '--ignored', '--nocapture']
    for piped in [False, True]:
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary).resolve()
            command = home / '.local/bin/neo'
            command.parent.mkdir(parents=True)
            command.write_bytes(b'preserve standalone neo')
            command.chmod(0o755)
            code, output = drive([installer, 'install'], home, piped=piped)
            assert code == 1, output
            assert str(command).encode() in output, output
            assert b'existing command' in output, output
            assert b'could not fully restore' not in output, output
            assert command.read_bytes() == b'preserve standalone neo'
            assert b'\x1b[?1049h' in output and b'\x1b[?1049l' in output, output
            print(f'PASS native conflict, piped stdin={piped}, terminal restored')
    for mode, action, expected in [('normal', b'\r', 'complete'),
                                   ('normal', b'q', 'cancelled'),
                                   ('signal-restore-primary', b'p', 'cancelled')]:
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary).resolve()
            code, output = drive(fixture, home, mode, action, piped=True)
            assert code == 0, output
            result = json.loads((home / 'result.json').read_text())
            assert result.get('outcome', result.get('operation')) == expected, result
            if mode == 'signal-restore-primary':
                assert result['primary'] == 'panic', result
                assert result['foreign_action_preserved'], result
                assert result['cleanup'] == ['signal_handlers'], result
                assert result['restored'], result
                assert output.count(b'could not restore its signal handlers') == 1, output
                assert b'could not fully restore the terminal' not in output, output
            print(f'PASS fixture {mode} {expected}, terminal restored')


if __name__ == '__main__':
    main()
