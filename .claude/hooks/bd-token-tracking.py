#!/usr/bin/env python3
"""PostToolUse hook (matcher: Bash): record token usage on beads.

- `bd update ... --claim`  -> snapshot the session's cumulative token count
  into the bead's metadata (tokens_start / tokens_out_start / tokens_session).
- `bd close ...` (or `bd update ... --status closed`) -> write tokens /
  tokens_out into metadata. If the claim snapshot came from this same
  session the value is the delta since claim; otherwise it falls back to
  the whole-session total and marks tokens_approx=true.

Token counts come from the Claude Code transcripts (input + output + cache
tokens, deduped by API message id since streaming repeats usage lines).
Subagent/workflow transcripts under <session-dir>/**/*.jsonl are included,
so the recorded cost covers the whole task, agents included.
tokens_scope says what the number covers: "delta" (since the claim in this
same session) or "session" (whole session — the right value when the
dispatcher runs one task per session).

Never blocks: any failure exits 0 silently so bd workflows are unaffected.
Canonical copy lives in the dispatcher repo; install elsewhere with
scripts/install-bd-token-tracking.sh.
"""

import glob
import json
import os
import re
import shlex
import subprocess
import sys

ID_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_.]*-[A-Za-z0-9]+$")
SHELL_OPS = {"&&", "||", ";", "|", "&"}
# bd global flags (before the subcommand) that consume a value
GLOBAL_VALUE_FLAGS = {"-C", "--directory", "--db", "--actor", "--dolt-auto-commit"}


def transcript_files(transcript_path):
    """Main transcript plus subagent/workflow transcripts of the session
    (they live under a directory named after the session id)."""
    files = [transcript_path]
    session_dir, ext = os.path.splitext(transcript_path)
    if ext == ".jsonl" and os.path.isdir(session_dir):
        files += glob.glob(
            os.path.join(session_dir, "**", "*.jsonl"), recursive=True
        )
    return files


def session_totals(transcript_path):
    usage_by_msg = {}
    for path in transcript_files(transcript_path):
        try:
            f = open(path, encoding="utf-8", errors="replace")
        except OSError:
            continue
        with f:
            for line in f:
                try:
                    entry = json.loads(line)
                except json.JSONDecodeError:
                    continue
                msg = entry.get("message") or {}
                usage = msg.get("usage")
                if not isinstance(usage, dict):
                    continue
                key = msg.get("id") or entry.get("requestId") or entry.get("uuid")
                usage_by_msg[key] = usage
    total = out = 0
    for u in usage_by_msg.values():
        o = u.get("output_tokens") or 0
        total += (
            (u.get("input_tokens") or 0)
            + (u.get("cache_creation_input_tokens") or 0)
            + (u.get("cache_read_input_tokens") or 0)
            + o
        )
        out += o
    return total, out


def bd(args, cwd):
    return subprocess.run(
        ["bd", *args], cwd=cwd, capture_output=True, text=True, timeout=20
    )


def parse_bd_invocations(command):
    """Yield (subcommand, ids, rest_words) for each bd close/update invocation.

    "bd" must sit at command position (segment start), so a "bd close x"
    inside an echo argument never matches. Lines are split before shlex —
    shlex treats newlines as plain whitespace and would merge separate
    commands into one argument stream. Issue ids are collected only between
    the subcommand and the first flag, which is how our own commands are
    shaped and avoids guessing which bd flags are boolean.
    """
    for line in command.splitlines():
        try:
            words = shlex.split(line)
        except ValueError:
            words = line.split()
        segments, seg = [], []
        for w in words:
            if w in SHELL_OPS:
                segments.append(seg)
                seg = []
            else:
                seg.append(w)
        segments.append(seg)
        for seg in segments:
            if not seg or seg[0] != "bd":
                continue
            i = 1
            while i < len(seg) and seg[i].startswith("-"):
                i += 2 if seg[i] in GLOBAL_VALUE_FLAGS else 1
            if i >= len(seg):
                continue
            sub = seg[i]
            if sub not in ("close", "update"):
                continue
            rest = seg[i + 1:]
            ids = []
            for w in rest:
                if w.startswith("-"):
                    break
                if ID_RE.fullmatch(w):
                    ids.append(w)
            yield sub, ids, rest


def is_close_update(rest):
    for k, w in enumerate(rest):
        if w in ("--status", "-s") and k + 1 < len(rest):
            return rest[k + 1] == "closed"
        if w in ("--status=closed", "-s=closed"):
            return True
    return False


def last_touched_id(cwd):
    r = bd(["show", "--current", "--json"], cwd)
    try:
        return json.loads(r.stdout)[0]["id"]
    except Exception:
        return None


def issue_metadata(issue, cwd):
    r = bd(["show", issue, "--json"], cwd)
    try:
        return json.loads(r.stdout)[0].get("metadata") or {}
    except Exception:
        return {}


def main():
    hook = json.load(sys.stdin)
    if hook.get("tool_name") != "Bash":
        return
    command = (hook.get("tool_input") or {}).get("command") or ""
    if "bd " not in command and not command.startswith("bd"):
        return
    transcript = hook.get("transcript_path")
    session = hook.get("session_id") or ""
    cwd = hook.get("cwd") or "."
    if not transcript:
        return

    actions = []
    for sub, ids, rest in parse_bd_invocations(command):
        if sub == "close" or (sub == "update" and is_close_update(rest)):
            actions.append(("close", ids))
        elif sub == "update" and "--claim" in rest:
            actions.append(("claim", ids))
    if not actions:
        return

    total, out = session_totals(transcript)

    for kind, ids in actions:
        if not ids:
            # no explicit id: bd operated on the last-touched issue
            lid = last_touched_id(cwd)
            ids = [lid] if lid else []
        for issue in ids:
            if kind == "claim":
                bd(
                    [
                        "update", issue,
                        "--set-metadata", f"tokens_start={total}",
                        "--set-metadata", f"tokens_out_start={out}",
                        "--set-metadata", f"tokens_session={session}",
                    ],
                    cwd,
                )
                continue
            meta = issue_metadata(issue, cwd)
            start = meta.get("tokens_start")
            out_start = meta.get("tokens_out_start") or 0
            if isinstance(start, int) and meta.get("tokens_session") == session:
                args = [
                    "update", issue,
                    "--set-metadata", f"tokens={total - start}",
                    "--set-metadata", f"tokens_out={out - out_start}",
                    "--set-metadata", "tokens_scope=delta",
                ]
            else:
                # no same-session claim snapshot: whole-session total
                args = [
                    "update", issue,
                    "--set-metadata", f"tokens={total}",
                    "--set-metadata", f"tokens_out={out}",
                    "--set-metadata", "tokens_scope=session",
                ]
            if "tokens_start" in meta:
                args += [
                    "--unset-metadata", "tokens_start",
                    "--unset-metadata", "tokens_out_start",
                    "--unset-metadata", "tokens_session",
                ]
            bd(args, cwd)


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass  # tracking must never break the bd workflow
    sys.exit(0)
