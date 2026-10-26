#!/usr/bin/env python3
import argparse
import json
import subprocess
import sys


def git(repo, *args, allow_no_matches=False):
    result = subprocess.run(
        ["git", "-C", repo, *args], capture_output=True, text=True
    )
    if result.returncode != 0 and not (allow_no_matches and result.returncode == 1):
        result.check_returncode()
    return result.stdout


def main():
    parser = argparse.ArgumentParser(
        description="Collect base/head lexical candidates for a release boundary review."
    )
    parser.add_argument("--repo", default=".")
    parser.add_argument("--base", required=True)
    parser.add_argument("--head", default="HEAD")
    parser.add_argument(
        "--pattern", required=True,
        help="Git extended regex for changed symbols, app entrypoints, and import aliases.",
    )
    args = parser.parse_args()
    revisions = {
        label: git(args.repo, "rev-parse", "--verify", f"{ref}^{{commit}}").strip()
        for label, ref in (("base", args.base), ("head", args.head))
    }
    report = {
        **revisions,
        "pattern": args.pattern,
        "changed_files": git(
            args.repo, "diff", "--name-status", revisions["base"], revisions["head"], "--"
        ),
        "candidates": {
            label: git(
                args.repo, "grep", "-n", "-I", "-E", "-e", args.pattern, revision,
                "--", ".", allow_no_matches=True,
            )
            for label, revision in revisions.items()
        },
        "limits": "Tracked text at committed revisions only. Lexical matches are not "
                  "a call graph, support policy, impact verdict, or compilation evidence. "
                  "Read surrounding source and trace aliases, reexports, and generated wiring.",
    }
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        print(error.stderr.strip(), file=sys.stderr)
        sys.exit(error.returncode)
