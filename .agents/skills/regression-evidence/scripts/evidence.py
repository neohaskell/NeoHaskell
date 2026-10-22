#!/usr/bin/env python3
from __future__ import annotations

import argparse
from dataclasses import dataclass
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
BEST_PRACTICES_SCRIPTS = Path(__file__).resolve().parents[2] / 'neohaskell-best-practices' / 'scripts'
sys.path.insert(0, str(BEST_PRACTICES_SCRIPTS))
from bounded_process import ProcessOutcome, run_bounded

SUMMARY = re.compile(r'^(\d+) examples?, (\d+) failures?\s*$', re.MULTILINE)
SETUP = ('Failed to build', 'Compilation failed', 'Connection refused', 'could not connect', 'TIMEOUT:')
TIMEOUT_MARKERS = ('test-match: FAIL: child process timed out',)
MAX_OUTPUT_BYTES = 32 * 1024 * 1024


@dataclass(frozen=True)
class RunResult:
    command: list[str]
    returncode: int | None
    text: str
    process_outcome: str
    elapsed_seconds: float
    runner_outcome: str | None
    requires_runner_receipt: bool
    runner_receipt_path: Path | None
    runner_receipt_sha256: str | None


def classify_runtime(code, output, expected, test, failure=None, *, runner_outcome=None, process_outcome='exited', requires_runner_receipt=False):
    output = re.sub(r'\x1b\[[0-9;]*m', '', output)
    if process_outcome != ProcessOutcome.EXITED.value:
        raise ValueError(f'Runtime process outcome `{process_outcome}` is not assertion evidence')
    if any(marker in output for marker in TIMEOUT_MARKERS):
        raise ValueError('A timed-out focused test is not runtime assertion evidence')
    if requires_runner_receipt:
        expected_runner_outcome = 'passed' if expected == 'pass' else 'assertion-failure'
        if runner_outcome != expected_runner_outcome:
            raise ValueError(f'Focused test outcome `{runner_outcome}` does not prove expected `{expected}` behavior')
    summaries = SUMMARY.findall(output)
    if len(summaries) != 1 or int(summaries[0][0]) < 1:
        raise ValueError('Runtime proof requires one nonempty Hspec selection')
    examples, failures = map(int, summaries[0])
    if any(marker in output for marker in SETUP):
        raise ValueError('Setup or compilation failure is not a runtime assertion proof')
    if expected == 'pass':
        if code != 0 or failures != 0:
            raise ValueError('Expected a successful runtime regression check')
    elif expected == 'fail':
        blocks = re.findall(r'^\s*\d+\)\s+([^\n]+)\n(.*?)(?=^\s*To rerun)', output, flags=re.MULTILINE | re.DOTALL)
        if code != 1 or failures != 1 or not failure or len(blocks) != 1:
            raise ValueError('Expected one identified runtime assertion failure')
        if test not in blocks[0][0] or blocks[0][1].count(failure) != 1 or output.count(failure) != 1:
            raise ValueError('Failure does not match the selected test and assertion')
    else:
        raise ValueError('Runtime expectation must be pass or fail')
    return {'kind': 'runtime-assertion', 'expected': expected, 'selected_examples': examples, 'failures': failures}


def classify_compiler(
    positive_code,
    negative_code,
    output,
    negative,
    diagnostic,
    *,
    positive_process_outcome=ProcessOutcome.EXITED.value,
    negative_process_outcome=ProcessOutcome.EXITED.value,
):
    if positive_process_outcome != ProcessOutcome.EXITED.value:
        raise ValueError(f'Positive compiler process outcome `{positive_process_outcome}` is not acceptance evidence')
    if negative_process_outcome != ProcessOutcome.EXITED.value:
        raise ValueError(f'Negative compiler process outcome `{negative_process_outcome}` is not rejection evidence')
    if positive_code != 0:
        raise ValueError('Positive consumer did not compile')
    if negative_code != 1 or str(negative) not in output or not re.search(r'\berror\b', output, re.I):
        raise ValueError('Negative consumer did not produce its own compiler error')
    if not re.search(diagnostic, output, re.MULTILINE):
        raise ValueError('Compiler error differs from the expected diagnostic')
    return {'kind': 'compiler-accept-reject', 'positive': 'accepted', 'negative': 'rejected', 'diagnostic': diagnostic}


def _test_match_location(command):
    for index, value in enumerate(command):
        if Path(value).name in {'test-match', 'test-match.py'}:
            return index
    return None


def _test_match_receipt(command, logfile):
    index = _test_match_location(command)
    if index is None:
        return command, None, False
    args = []
    cursor = 0
    original_args = command[index + 1:]
    while cursor < len(original_args):
        value = original_args[cursor]
        if value == '--':
            args.extend(original_args[cursor:])
            break
        if value == '--transcript':
            cursor += 2
            continue
        if value.startswith('--transcript='):
            cursor += 1
            continue
        args.append(value)
        cursor += 1
    transcript = logfile.resolve().with_name(logfile.name + '.test-match')
    command = [*command[:index + 1], '--transcript', str(transcript), *args]
    return command, transcript, True


def _read_runner_outcome(path):
    try:
        header = path.read_text(errors='replace').split('--- output ---', 1)[0]
    except OSError:
        return None
    rows = re.findall(r'^outcome: ([a-z-]+)$', header, re.MULTILINE)
    if len(rows) != 1 or rows[0] not in {'passed', 'assertion-failure', 'timed-out', 'output-limited', 'process-failure', 'setup-error', 'invalid-summary', 'empty-selection'}:
        return None
    return rows[0]


def run(command, root, logfile, timeout, environment=None):
    if not command or any(not isinstance(value, str) or '\0' in value for value in command):
        raise ValueError('Expected a command argument array')
    logfile.parent.mkdir(parents=True, exist_ok=True)
    command, runner_transcript, requires_runner_receipt = _test_match_receipt(command, logfile)
    result = run_bounded(
        command,
        cwd=root,
        env=environment,
        timeout=timeout,
        max_output_bytes=MAX_OUTPUT_BYTES,
    )
    logfile.write_bytes(result.output)
    runner_outcome = _read_runner_outcome(runner_transcript) if runner_transcript else None
    runner_receipt_sha256 = (
        hashlib.sha256(runner_transcript.read_bytes()).hexdigest()
        if runner_transcript and runner_transcript.is_file()
        else None
    )
    return RunResult(
        command=command,
        returncode=result.returncode,
        text=result.output.decode('utf-8', errors='replace'),
        process_outcome=result.outcome.value,
        elapsed_seconds=result.elapsed_seconds,
        runner_outcome=runner_outcome,
        requires_runner_receipt=requires_runner_receipt,
        runner_receipt_path=runner_transcript if runner_receipt_sha256 else None,
        runner_receipt_sha256=runner_receipt_sha256,
    )


def source_inputs(root, names):
    result = {}
    for name in names:
        path = (root / name).resolve()
        if not path.is_relative_to(root) or not path.is_file():
            raise ValueError(f'Input must be a file in the selected checkout: {name}')
        relative = str(path.relative_to(root))
        result[relative] = hashlib.sha256(path.read_bytes()).hexdigest()
    if not result:
        raise ValueError('Name the source inputs this run covers')
    return result


def capture(args):
    root = args.repo.resolve() if args.repo else Path(subprocess.check_output(['git', '-C', str(Path(__file__).parent), 'rev-parse', '--show-toplevel'], text=True).strip())
    names = list(args.input)
    if args.mode == 'compiler':
        names += [str(args.positive), str(args.negative)]
        re.compile(args.diagnostic)
    before = source_inputs(root, names)
    parent = root / '.agents-state/regression-evidence'
    parent.mkdir(parents=True, exist_ok=True)
    output = Path(tempfile.mkdtemp(prefix=args.mode + '-', dir=parent)).resolve()
    for name in before:
        target = output / 'source' / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((root / name).read_bytes())
    git = subprocess.run(['git', '-C', str(root), 'rev-parse', 'HEAD'], capture_output=True, text=True, timeout=5)
    receipt = {'schema': 1, 'root': str(root), 'head': git.stdout.strip() if git.returncode == 0 else None, 'status': 'incomplete', 'source_inputs': before, 'commands': []}
    try:
        if args.mode == 'runtime':
            command = args.command[1:] if args.command[:1] == ['--'] else args.command
            result = run(command, root, output / 'runtime.log', args.timeout)
            receipt['commands'].append({
                'argv': result.command,
                'exit': result.returncode,
                'process_outcome': result.process_outcome,
                'runner_outcome': result.runner_outcome,
                'elapsed_seconds': result.elapsed_seconds,
                'runner_receipt': str(result.runner_receipt_path.relative_to(output)) if result.runner_receipt_path else None,
                'runner_receipt_sha256': result.runner_receipt_sha256,
                'log': 'runtime.log',
            })
            receipt['observation'] = classify_runtime(
                result.returncode,
                result.text,
                args.expect,
                args.test,
                args.failure,
                runner_outcome=result.runner_outcome,
                process_outcome=result.process_outcome,
                requires_runner_receipt=result.requires_runner_receipt,
            )
            if args.expect == 'pass' and args.test not in result.command and args.test not in result.text:
                raise ValueError('Passing run does not identify the selected regression test')
        else:
            helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
            if not helper.is_file():
                raise ValueError(f'Toolchain helper is missing: {helper}')
            flags = args.command[1:] if args.command[:1] == ['--'] else args.command
            positive = (root / args.positive).resolve()
            negative = (root / args.negative).resolve()
            environment = {**os.environ, 'NEOHASKELL_ROOT': str(root)}
            compiler_runs = {}
            for label, source in [('positive', positive), ('negative', negative)]:
                build = output / (label + '-build')
                build.mkdir()
                command = [str(helper), 'ghc', '-fno-code', '-fforce-recomp', '-outputdir', str(build), *flags, str(source)]
                result = run(command, root, output / (label + '.log'), args.timeout, environment)
                compiler_runs[label] = result
                receipt['commands'].append({
                    'argv': result.command,
                    'exit': result.returncode,
                    'process_outcome': result.process_outcome,
                    'elapsed_seconds': result.elapsed_seconds,
                    'log': label + '.log',
                })
                if label == 'positive' and result.returncode != 0:
                    raise ValueError('Positive consumer did not compile; inspect positive.log')
            receipt['observation'] = classify_compiler(
                compiler_runs['positive'].returncode,
                compiler_runs['negative'].returncode,
                compiler_runs['negative'].text,
                negative,
                args.diagnostic,
                positive_process_outcome=compiler_runs['positive'].process_outcome,
                negative_process_outcome=compiler_runs['negative'].process_outcome,
            )
        if source_inputs(root, names) != before:
            raise ValueError('Selected source inputs changed during execution')
        for name in before:
            if hashlib.sha256((output / 'source' / name).read_bytes()).hexdigest() != before[name]:
                raise ValueError('Source snapshot changed before execution')
        receipt['log_hashes'] = {row['log']: hashlib.sha256((output / row['log']).read_bytes()).hexdigest() for row in receipt['commands']}
        for row in receipt['commands']:
            if row.get('runner_receipt'):
                receipt['log_hashes'][row['runner_receipt']] = row['runner_receipt_sha256']
        receipt['status'] = 'observed'
    except (ValueError, OSError) as error:
        receipt['error'] = str(error)
        raise
    finally:
        (output / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
        print(output / 'receipt.json')
    return receipt


def main():
    parser = argparse.ArgumentParser(description='Capture distinct runtime-assertion and compiler acceptance/rejection evidence.')
    modes = parser.add_subparsers(dest='mode', required=True)
    for name in ('runtime', 'compiler'):
        mode = modes.add_parser(name)
        mode.add_argument('--repo', type=Path)
        mode.add_argument('--input', action='append', default=[])
        mode.add_argument('--timeout', type=float, default=180)
        if name == 'runtime':
            mode.add_argument('--test', required=True)
            mode.add_argument('--expect', choices=['pass', 'fail'], required=True)
            mode.add_argument('--failure')
        else:
            mode.add_argument('--positive', required=True, type=Path)
            mode.add_argument('--negative', required=True, type=Path)
            mode.add_argument('--diagnostic', required=True)
        mode.add_argument('command', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if not 0 < args.timeout < float('inf'):
        parser.error('timeout must be finite and positive')
    try:
        capture(args)
    except (ValueError, OSError, re.error) as error:
        print(f'regression-evidence: {error}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
