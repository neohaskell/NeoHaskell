#!/usr/bin/env python3
"""Trusted validation guard for the maintainer codemap-regeneration workflow
(.github/workflows/codemap-regen.yml). Pure stdlib, no toolchain.

The `publish` job runs NO contributor code — it calls these pure, fail-closed
decisions to gate every write onto a reviewed contributor PR branch. Each
subcommand is a thin wrapper around a pure function that `--self-test` exercises
with blocking + passing cases (run by `./dev doctor`):

  check-metadata --pinned F --fresh F [--base main] [--base-repo owner/name]
      Compare resolve-time pinned PR metadata against a fresh re-fetch and
      enforce editability: the pinned/fresh head SHA·repo·ref·state·base must be
      identical, the PR must be `open` against the expected base, and a FORK head
      must be user-owned with `maintainer_can_modify=true`. Exit 0 ok /
      1 changed-or-uneditable. Used both before generation and immediately before
      the push (the remote-head recheck, criteria C1 + the C7 pre-push gate).

  check-symlinks ROOT
      Reject any symlink at or under ROOT/codemap (including the `codemap` and
      `codemap/signatures` path components). Exit 0 clean / 1 symlink found. (C4)

  reconcile --artifact DIR --manifest F --worktree DIR
      Validate the generated manifest is within the codemap allowlist and matches
      the artifact, then emit the apply plan on stdout: `copy <path>` per
      allowlisted artifact file, `delete <path>` per allowlisted signature tracked
      in the worktree but absent from the manifest (stale removal/rename). Exit
      0 ok / 1 rejected (manifest/artifact entry outside the allowlist, or a
      manifest/artifact mismatch). (C5)

  check-staged-diff F
      F holds `git diff --cached --name-status` output. Exit 0 = an allowlisted
      change is staged (commit), 3 = empty (successful no-op, no commit), 1 = a
      staged path is outside the codemap allowlist (fail). (C6)

  --self-test   embedded blocking + passing cases (doctor/CI).

Run:  ./dev codemap-regen-guard <subcommand> …     ./dev codemap-regen-guard --self-test
"""

import argparse
import json
import os
import re
import sys

sys.dont_write_bytecode = True  # no scripts/__pycache__/ (doctor scans scripts/)

# ── the generated-output allowlist (exactly what `./dev codemap` writes and git
#    tracks; the hoogle `.hoogle-*` DBs are gitignored and never collected) ────
ALLOW_EXACT = ("codemap/MAP.md", "codemap/.doc-ratchet")
SIG_DIR = "codemap/signatures"
# a signature file: one component below codemap/signatures/, `<stem>.txt`, stem
# starting with an alphanumeric (so `.txt`, dotfiles, and `..` never qualify).
SIG_NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]*\.txt")

# metadata fields that must be byte-identical between the pinned snapshot and a
# fresh re-fetch (a change in any of them is a TOCTOU race → fail closed).
PINNED_FIELDS = ("head_sha", "head_repo", "head_ref", "state", "base_ref")


def is_allowlisted(path):
    """True iff `path` (a repo-relative, forward-slash path) is a generated
    codemap output we are permitted to write. Rejects absolute paths, `..`,
    symlink-y components, and anything outside the three known outputs."""
    if not path or path != path.strip():
        return False
    if path.startswith("/") or "\\" in path or ".." in path.split("/"):
        return False
    if path in ALLOW_EXACT:
        return True
    head, sep, name = path.rpartition("/")
    return bool(head == SIG_DIR and sep == "/" and SIG_NAME.fullmatch(name))


# ── check-metadata (C1 + C7 pre-push recheck) ────────────────────────────────
def metadata_errors(pinned, fresh, base="main", base_repo=None):
    """[errors] when the fresh re-fetch diverged from the pinned snapshot or the
    PR is not a safely-editable, open, correctly-based target. Pure."""
    errs = []
    for f in PINNED_FIELDS:
        if pinned.get(f) != fresh.get(f):
            errs.append(f"metadata changed since pin: {f} "
                        f"{pinned.get(f)!r} -> {fresh.get(f)!r}")
    if fresh.get("state") != "open":
        errs.append(f"PR is not open (state={fresh.get('state')!r})")
    if fresh.get("base_ref") != base:
        errs.append(f"PR base is {fresh.get('base_ref')!r}, expected {base!r}")
    head_repo = fresh.get("head_repo")
    is_fork = bool(base_repo) and head_repo != base_repo
    if is_fork:
        if fresh.get("head_owner_type") != "User":
            errs.append(f"fork head {head_repo!r} is not user-owned "
                        "(org-owned/unsupported) — no fallback")
        if fresh.get("maintainer_can_modify") is not True:
            errs.append(f"maintainer edits are not enabled on fork {head_repo!r}")
    return errs


# ── check-symlinks (C4) ──────────────────────────────────────────────────────
def symlink_offenders(root):
    """Repo-relative paths under `root/codemap` that are symlinks (including the
    `codemap` dir entry itself). Walks the filesystem; does not follow links."""
    offenders = []
    codemap = os.path.join(root, "codemap")
    if os.path.islink(codemap):
        return ["codemap"]
    if not os.path.isdir(codemap):
        return offenders
    for dirpath, dirnames, filenames in os.walk(codemap, followlinks=False):
        for nm in list(dirnames) + filenames:
            full = os.path.join(dirpath, nm)
            if os.path.islink(full):
                offenders.append(os.path.relpath(full, root).replace(os.sep, "/"))
    return sorted(offenders)


# ── reconcile (C5) ───────────────────────────────────────────────────────────
def reconcile_plan(manifest_paths, artifact_paths, worktree_sig_paths):
    """(plan, errors). plan = {'copy': [...], 'delete': [...]}. Pure.

    - every manifest and artifact entry must be allowlisted;
    - the artifact must be exactly the manifest set (no unlisted/again-missing);
    - deletes = allowlisted signatures tracked in the worktree but absent from
      the manifest (a stale/renamed `codemap/signatures/*.txt`). Non-allowlisted
      files that merely live under `codemap/signatures/` (e.g. a `notes.md`) are
      NOT ours to touch — they are filtered out and never planned for deletion."""
    errs = []
    for p in manifest_paths:
        if not is_allowlisted(p):
            errs.append(f"manifest entry outside allowlist: {p!r}")
    for p in artifact_paths:
        if not is_allowlisted(p):
            errs.append(f"artifact file outside allowlist: {p!r}")
    manifest_set = set(manifest_paths)
    artifact_set = set(artifact_paths)
    if len(manifest_paths) != len(manifest_set):
        errs.append("manifest has duplicate entries")
    for p in artifact_set - manifest_set:
        errs.append(f"artifact file not listed in manifest: {p!r}")
    for p in manifest_set - artifact_set:
        errs.append(f"manifest lists a file absent from the artifact: {p!r}")
    if errs:
        return None, errs
    deletes = sorted(p for p in worktree_sig_paths
                     if is_allowlisted(p) and p not in manifest_set)
    return {"copy": sorted(artifact_set), "delete": deletes}, []


# ── check-staged-diff (C6) ───────────────────────────────────────────────────
def staged_diff_verdict(name_status_lines):
    """(exit_code, offending_paths). 0 = allowlisted change → commit, 3 = empty
    → no-op success, 1 = a staged path outside the allowlist → fail. Pure.

    Each line is `git diff --cached --name-status`: `<STATUS>\\t<path>` (renames/
    copies carry two paths, `R<score>\\t<old>\\t<new>`) — every path must be
    allowlisted."""
    paths = []
    for line in name_status_lines:
        line = line.rstrip("\n")
        if not line.strip():
            continue
        parts = line.split("\t")
        paths.extend(p for p in parts[1:] if p)
    bad = [p for p in paths if not is_allowlisted(p)]
    if bad:
        return 1, bad
    if not paths:
        return 3, []
    return 0, []


# ── filesystem/scan wrappers (thin; the cores above are what's tested) ────────
def _read_json(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def _scan_repo_relative(root, subdir):
    """Repo-relative forward-slash paths of regular files under root/subdir."""
    base = os.path.join(root, subdir)
    out = []
    if not os.path.isdir(base):
        return out
    for dirpath, _dirs, files in os.walk(base, followlinks=False):
        for nm in files:
            full = os.path.join(dirpath, nm)
            if os.path.islink(full):
                continue
            out.append(os.path.relpath(full, root).replace(os.sep, "/"))
    return out


def cmd_check_metadata(args):
    errs = metadata_errors(_read_json(args.pinned), _read_json(args.fresh),
                           base=args.base, base_repo=args.base_repo)
    if errs:
        print("codemap-regen-guard: metadata check FAILED", file=sys.stderr)
        for e in errs:
            print(f"  - {e}", file=sys.stderr)
        return 1
    print("codemap-regen-guard: metadata OK (pinned == fresh, open, editable)")
    return 0


def cmd_check_symlinks(args):
    offenders = symlink_offenders(args.root)
    if offenders:
        print("codemap-regen-guard: symlink(s) under codemap/ — rejected",
              file=sys.stderr)
        for o in offenders:
            print(f"  - {o}", file=sys.stderr)
        return 1
    print("codemap-regen-guard: no symlinks under codemap/")
    return 0


def cmd_reconcile(args):
    with open(args.manifest, encoding="utf-8") as f:
        manifest = [ln.strip() for ln in f if ln.strip()]
    artifact = _scan_repo_relative(args.artifact, "codemap")
    worktree_sigs = [p for p in _scan_repo_relative(args.worktree, SIG_DIR)]
    plan, errs = reconcile_plan(manifest, artifact, worktree_sigs)
    if errs:
        print("codemap-regen-guard: reconcile FAILED", file=sys.stderr)
        for e in errs:
            print(f"  - {e}", file=sys.stderr)
        return 1
    for p in plan["copy"]:
        print(f"copy {p}")
    for p in plan["delete"]:
        print(f"delete {p}")
    return 0


def cmd_check_staged_diff(args):
    with open(args.file, encoding="utf-8") as f:
        code, bad = staged_diff_verdict(f.readlines())
    if code == 1:
        print("codemap-regen-guard: staged diff has non-codemap path(s) — fail",
              file=sys.stderr)
        for p in bad:
            print(f"  - {p}", file=sys.stderr)
    elif code == 3:
        print("codemap-regen-guard: staged diff empty — no-op success")
    else:
        print("codemap-regen-guard: staged diff is codemap-only — commit")
    return code


# ── self-test ────────────────────────────────────────────────────────────────
def self_test():
    import tempfile
    ok = True

    def check(name, cond):
        nonlocal ok
        print(f"  {'ok  ' if cond else 'FAIL'} {name}")
        ok = ok and cond

    # allowlist
    check("MAP.md allowlisted", is_allowlisted("codemap/MAP.md"))
    check("ratchet dotfile allowlisted", is_allowlisted("codemap/.doc-ratchet"))
    check("signature txt allowlisted",
          is_allowlisted("codemap/signatures/nhcore-core.txt"))
    check("absolute path rejected", not is_allowlisted("/etc/passwd"))
    check("dotdot rejected", not is_allowlisted("codemap/../.git/config"))
    check("nested sig rejected",
          not is_allowlisted("codemap/signatures/a/b.txt"))
    check("non-txt sig rejected",
          not is_allowlisted("codemap/signatures/evil.sh"))
    check("bare .txt rejected", not is_allowlisted("codemap/signatures/.txt"))
    check("outside-codemap rejected", not is_allowlisted("core/core/Text.hs"))
    check("backslash rejected", not is_allowlisted("codemap\\MAP.md"))

    # ── C1: metadata pin/recheck + editability
    pinned = {"head_sha": "abc", "head_repo": "user/NeoHaskell",
              "head_ref": "feat", "state": "open", "base_ref": "main",
              "head_owner_type": "User", "maintainer_can_modify": True}
    base_repo = "neohaskell/NeoHaskell"
    check("identical editable fork passes",
          not metadata_errors(pinned, dict(pinned), base_repo=base_repo))
    for f in PINNED_FIELDS:
        moved = dict(pinned)
        moved[f] = "CHANGED"
        # keep state/base otherwise valid so we isolate the change detection
        if f == "state":
            moved["state"] = "closed"
        if f == "base_ref":
            moved["base_ref"] = "release"
        check(f"changed {f} fails",
              bool(metadata_errors(pinned, moved, base_repo=base_repo)))
    closed = dict(pinned, state="closed")
    check("closed PR fails",
          any("not open" in e for e in metadata_errors(pinned, closed, base_repo=base_repo)))
    wrongbase = dict(pinned, base_ref="release")
    check("wrong base fails",
          any("base" in e for e in metadata_errors(pinned, wrongbase, base_repo=base_repo)))
    noedit = dict(pinned, maintainer_can_modify=False)
    check("fork without maintainer-edits fails",
          any("maintainer edits" in e for e in metadata_errors(pinned, noedit, base_repo=base_repo)))
    orgfork = dict(pinned, head_owner_type="Organization")
    check("org-owned fork fails",
          any("user-owned" in e for e in metadata_errors(pinned, orgfork, base_repo=base_repo)))
    same = {"head_sha": "abc", "head_repo": base_repo, "head_ref": "feat",
            "state": "open", "base_ref": "main", "head_owner_type": "Organization",
            "maintainer_can_modify": False}
    check("same-repo PR needs no maintainer-edits",
          not metadata_errors(same, dict(same), base_repo=base_repo))

    # ── C4: symlink rejection
    with tempfile.TemporaryDirectory() as d:
        os.makedirs(os.path.join(d, "codemap", "signatures"))
        with open(os.path.join(d, "codemap", "MAP.md"), "w") as f:
            f.write("x")
        check("clean tree: no symlink offenders", not symlink_offenders(d))
        os.symlink("/etc", os.path.join(d, "codemap", "signatures", "evil"))
        check("symlink under codemap flagged",
              symlink_offenders(d) == ["codemap/signatures/evil"])
    with tempfile.TemporaryDirectory() as d:
        os.symlink("/tmp", os.path.join(d, "codemap"))
        check("codemap component itself a symlink flagged",
              symlink_offenders(d) == ["codemap"])

    # ── C5: reconcile add / delete / out-of-allowlist
    manifest = ["codemap/MAP.md", "codemap/.doc-ratchet",
                "codemap/signatures/nhcore-core.txt"]
    plan, errs = reconcile_plan(
        manifest, list(manifest),
        worktree_sig_paths=["codemap/signatures/nhcore-core.txt",
                            "codemap/signatures/stale.txt"])
    check("reconcile ok: no errors", not errs)
    check("reconcile copies the artifact set",
          plan and plan["copy"] == sorted(manifest))
    check("reconcile deletes stale signature absent from manifest",
          plan and plan["delete"] == ["codemap/signatures/stale.txt"])
    plan_nm, errs_nm = reconcile_plan(
        manifest, list(manifest),
        worktree_sig_paths=["codemap/signatures/nhcore-core.txt",
                            "codemap/signatures/stale.txt",
                            "codemap/signatures/notes.md"])
    check("reconcile ignores a non-allowlisted file under signatures/ "
          "(never deletes notes.md) while still deleting the stale .txt",
          not errs_nm and plan_nm
          and plan_nm["delete"] == ["codemap/signatures/stale.txt"])
    _, errs2 = reconcile_plan(
        manifest + ["core/core/Text.hs"], list(manifest), [])
    check("reconcile rejects out-of-allowlist manifest entry",
          any("outside allowlist" in e for e in errs2))
    _, errs3 = reconcile_plan(
        manifest, list(manifest) + ["codemap/extra.txt"], [])
    check("reconcile rejects out-of-allowlist artifact file",
          bool(errs3))
    _, errs4 = reconcile_plan(
        manifest, ["codemap/MAP.md"], [])  # artifact missing manifest entries
    check("reconcile rejects manifest/artifact mismatch",
          any("absent from the artifact" in e for e in errs4))

    # ── C6: staged-diff verdict
    code, _ = staged_diff_verdict(["M\tcodemap/MAP.md",
                                   "A\tcodemap/signatures/new.txt"])
    check("allowlisted staged diff -> commit (0)", code == 0)
    code, _ = staged_diff_verdict([])
    check("empty staged diff -> no-op (3)", code == 3)
    code, bad = staged_diff_verdict(["M\tcore/core/Text.hs"])
    check("non-allowlisted staged path -> fail (1)",
          code == 1 and bad == ["core/core/Text.hs"])
    code, _ = staged_diff_verdict(
        ["R100\tcodemap/signatures/old.txt\tcodemap/signatures/new.txt"])
    check("rename within allowlist -> commit (0)", code == 0)
    code, bad = staged_diff_verdict(
        ["R100\tcodemap/signatures/old.txt\tevil/x.txt"])
    check("rename escaping allowlist -> fail (1)", code == 1)

    print("codemap-regen-guard: self-test", "OK" if ok else "FAILED")
    return 0 if ok else 1


def main(argv):
    p = argparse.ArgumentParser(prog="codemap-regen-guard", add_help=True)
    p.add_argument("--self-test", action="store_true")
    sub = p.add_subparsers(dest="cmd")

    m = sub.add_parser("check-metadata")
    m.add_argument("--pinned", required=True)
    m.add_argument("--fresh", required=True)
    m.add_argument("--base", default="main")
    m.add_argument("--base-repo", default="neohaskell/NeoHaskell")

    s = sub.add_parser("check-symlinks")
    s.add_argument("root")

    r = sub.add_parser("reconcile")
    r.add_argument("--artifact", required=True)
    r.add_argument("--manifest", required=True)
    r.add_argument("--worktree", required=True)

    d = sub.add_parser("check-staged-diff")
    d.add_argument("file")

    args = p.parse_args(argv)
    if args.self_test:
        return self_test()
    if args.cmd == "check-metadata":
        return cmd_check_metadata(args)
    if args.cmd == "check-symlinks":
        return cmd_check_symlinks(args)
    if args.cmd == "reconcile":
        return cmd_reconcile(args)
    if args.cmd == "check-staged-diff":
        return cmd_check_staged_diff(args)
    p.print_help(sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
