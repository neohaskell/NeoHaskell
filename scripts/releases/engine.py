"""Deterministic release planning. No GitHub writes and no model calls."""

import datetime
import hashlib
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]
CONFIG_PATH = "scripts/releases/config.json"
LEDGER_PATH = "scripts/releases/manifest.json"
CONFIG = json.loads((ROOT / CONFIG_PATH).read_text())
REPOSITORY = CONFIG["repository"]
TARGETS = (
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "aarch64-apple-darwin",
)
CATEGORIES = ("Breaking changes", "Added", "Improved", "Fixed", "Deprecated", "Removed")
IMPACTS = {"none": 0, "compatible": 1, "breaking": 2}
COMPONENTS = {
    "platform": {"Framework", "Integrations", "CLI", "IDE"},
    "installer": {"Installer"},
}
SHA = re.compile(r"[0-9a-f]{40}")


def canonical(value):
    return json.dumps(value, sort_keys=True, ensure_ascii=False, indent=2) + "\n"


def digest(value):
    return hashlib.sha256(
        value.encode() if isinstance(value, str) else value
    ).hexdigest()


def version(value):
    if not re.fullmatch(r"0\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)", value):
        raise ValueError(f"Expected a stable pre-1.0 version, got {value!r}")
    return tuple(map(int, value.split(".")))


def bump(previous, impact):
    _, major, patch = version(previous)
    if impact not in IMPACTS:
        raise ValueError("Unknown release impact")
    if impact == "none":
        return previous
    return f"0.{major + 1}.0" if impact == "breaking" else f"0.{major}.{patch + 1}"


def commit_impact(message):
    if re.match(r"\w+(?:\([^)]+\))?!:", message) or re.search(
        r"^BREAKING[ -]CHANGE:", message, re.M
    ):
        return "breaking"
    if re.match(r"(feat|fix|perf)(?:\([^)]+\))?:", message):
        return "compatible"
    return "none"


def sections(body):
    result, name, lines, fence = {}, None, [], None
    for line in body.splitlines():
        marker = re.match(r"^\s*(`{3,}|~{3,})", line)
        if marker:
            token = marker[1]
            if fence is None:
                fence = token
            elif token[0] == fence[0] and len(token) >= len(fence):
                fence = None
        heading = (
            re.fullmatch(r"## (Summary|Migration|Agent prompt)", line)
            if fence is None
            else None
        )
        if heading:
            if name is not None:
                result[name] = "\n".join(lines).strip()
            name, lines = heading[1], []
            if name in result:
                raise ValueError(f"Duplicate section {name}")
        else:
            lines.append(line)
    if fence:
        raise ValueError("Unclosed Markdown fence")
    if name:
        result[name] = "\n".join(lines).strip()
    return result


def substantive(text):
    return len(text.strip()) >= 20 and not re.search(
        r"\b(TODO|TBD|placeholder)\b|^(Explain|Provide) ", text, re.I
    )


def fragment(path, text):
    if not re.fullmatch(r"\.changes/[a-z0-9][a-z0-9-]*\.md", path):
        raise ValueError(f"Invalid release fragment path: {path}")
    match = re.match(r"\A---\n(.*?)\n---\n(.*)\Z", text, re.S)
    if not match:
        raise ValueError(f"{path}: expected frontmatter")
    meta = {}
    for line in match[1].splitlines():
        pair = re.fullmatch(r"([a-z]+): ([^\n]+)", line)
        if not pair or pair[1] in meta:
            raise ValueError(f"{path}: invalid/duplicate metadata")
        meta[pair[1]] = pair[2]
    if set(meta) != {"group", "component", "impact", "category"}:
        raise ValueError(f"{path}: expected group, component, impact, category")
    if (
        meta["group"] not in COMPONENTS
        or meta["component"] not in COMPONENTS[meta["group"]]
    ):
        raise ValueError(f"{path}: invalid component/group")
    if meta["impact"] not in IMPACTS or meta["category"] not in CATEGORIES:
        raise ValueError(f"{path}: invalid impact/category")
    parts = sections(match[2])
    if not substantive(parts.get("Summary", "")):
        raise ValueError(f"{path}: write a substantive summary/reason")
    if meta["category"] == "Breaking changes" and meta["impact"] != "breaking":
        raise ValueError(f"{path}: breaking category requires breaking impact")
    if meta["impact"] == "breaking":
        verification = re.search(
            r"(?ms)^### Verify\s*\n(.*?)(?=^### |\Z)", parts.get("Migration", "")
        )
        if (
            meta["category"] != "Breaking changes"
            or not substantive(parts.get("Migration", ""))
            or verification is None
            or not substantive(verification[1])
        ):
            raise ValueError(
                f"{path}: breaking change needs migration steps and verification"
            )
        prompt = parts.get("Agent prompt", "")
        if not substantive(prompt) or not re.search(
            r"^```(?:text)?\n.+?\n```$", prompt, re.M | re.S
        ):
            raise ValueError(
                f"{path}: breaking change needs a fenced, self-contained agent prompt"
            )
    return {**meta, "path": path, "text": text, "hash": digest(text), "sections": parts}


def release_tag(plan):
    return CONFIG["groups"][plan["group"]]["tag"] + plan["version"]


def render(plan):
    notes = [n for n in plan["notes"] if n["impact"] != "none"]
    rows = [f"## {plan['version']} — {plan['date']}"]
    for category in CATEGORIES:
        selected = [n for n in notes if n["category"] == category]
        if not selected:
            continue
        rows += ["", f"### {category}", ""]
        for note in selected:
            url = f"https://github.com/{REPOSITORY}/commit/{note['commit']}"
            rows.append(
                f"- **{note['component']}**: {note['sections']['Summary']} ([change]({url}))"
            )
    breaking = [n for n in notes if n["impact"] == "breaking"]
    if breaking:
        rows += ["", f"### Migration from {plan['base_version']}"]
        for note in breaking:
            prompt = note["sections"]["Agent prompt"]
            preamble = f"Migrate {note['component']} from NeoHaskell {plan['base_version']} to {plan['version']}.\n"
            prompt = re.sub(
                r"(^```(?:text)?\n)",
                lambda m: m[1] + preamble,
                prompt,
                count=1,
                flags=re.M,
            )
            rows += [
                "",
                f"#### {note['component']}",
                "",
                note["sections"]["Migration"],
                "",
                "<details>",
                "<summary>Copy this prompt to your coding agent</summary>",
                "",
                prompt,
                "",
                "</details>",
            ]
    rows += [
        "",
        f"[Compare changes](https://github.com/{REPOSITORY}/compare/{plan['base_tag']}...{release_tag(plan)})",
        "",
    ]
    return "\n".join(rows)


class Repo:
    def __init__(self, root=ROOT):
        self.root = Path(root)
        self.trees = {}
        self.blobs = {}

    def git(self, *args):
        return subprocess.check_output(["git", *args], cwd=self.root, text=True).strip()

    def revision(self, ref):
        if ref.startswith("-"):
            raise ValueError("Invalid revision")
        result = self.git("rev-parse", "--verify", ref + "^{commit}")
        if not SHA.fullmatch(result):
            raise ValueError("Invalid commit")
        return result

    def tree(self, rev):
        if SHA.fullmatch(rev) and rev in self.trees:
            return self.trees[rev]
        result = {}
        raw = subprocess.check_output(["git", "ls-tree", "-rz", rev], cwd=self.root)
        for item in raw.split(b"\0"):
            if not item:
                continue
            meta, path = item.split(b"\t", 1)
            mode, kind, oid = meta.decode().split()
            result[path.decode()] = (mode, oid)
        if SHA.fullmatch(rev):
            self.trees[rev] = result
        return result

    def read(self, rev, path):
        info = self.tree(rev).get(path)
        if info is None:
            return None
        if info[0] not in {"100644", "100755"}:
            raise ValueError(f"Not a regular file: {path}")
        if info[1] not in self.blobs:
            self.blobs[info[1]] = subprocess.check_output(
                ["git", "cat-file", "blob", info[1]], cwd=self.root
            ).decode()
        return self.blobs[info[1]]

    def ledger(self, rev):
        data = json.loads(
            self.read(rev, LEDGER_PATH)
            or canonical({"schema": 1, "attempts": [], "recoveries": []})
        )
        if set(data) != {"schema", "attempts", "recoveries"} or data["schema"] != 1:
            raise ValueError("Invalid release ledger")
        ids = set()
        for plan in data["attempts"] + data["recoveries"]:
            check_identity(plan)
            if plan["id"] in ids:
                raise ValueError("Duplicate release identity")
            ids.add(plan["id"])
        return data

    def commits(self, base, head):
        return list(
            reversed(
                self.git("rev-list", "--first-parent", f"{base}..{head}").splitlines()
            )
        )

    def ancestor(self, base, head):
        return (
            subprocess.run(
                ["git", "merge-base", "--is-ancestor", base, head],
                cwd=self.root,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            ).returncode
            == 0
        )

    def fragments(self, rev):
        notes = []
        for path in sorted(self.tree(rev)):
            if path.startswith(".changes/"):
                notes.append(fragment(path, self.read(rev, path)))
        return notes


def identify(data):
    return {**data, "id": digest(canonical(data))}


def check_identity(plan):
    base = {k: v for k, v in plan.items() if k != "id"}
    if plan.get("id") != digest(canonical(base)):
        raise ValueError("Release manifest identity mismatch")
    if plan.get("group") not in COMPONENTS or not SHA.fullmatch(
        plan.get("source_sha", "")
    ):
        raise ValueError("Invalid manifest group/source")


def abandoned(ledger):
    return {r["attempt"] for r in ledger["recoveries"]}


def note_origins(repo, source, notes, ledger):
    prior = {n["path"]: (p, n) for p in ledger["attempts"] for n in p["notes"]}
    for note in notes:
        # Recovery restores the same identity instead of inventing a second change.
        old = prior.get(note["path"])
        if old:
            attempt, old = old
            if attempt["id"] not in abandoned(ledger) or old["group"] != note["group"]:
                raise ValueError(
                    "Fragment path was already consumed; use a unique new path"
                )
            note["commit"] = old["commit"]
            note["original_hash"] = old.get("original_hash", old["hash"])
        else:
            commits = repo.git(
                "log",
                "--first-parent",
                "--diff-filter=A",
                "--format=%H",
                source,
                "--",
                note["path"],
            ).splitlines()
            if not commits:
                raise ValueError("Fragment must be committed before release planning")
            note["commit"] = commits[-1]
    return notes


def plan_release(repo, source, group, completed, date, bootstrap=None, check_tags=True):
    """completed maps attempt IDs to verified public release SHAs, supplied by the publisher adapter."""
    source = repo.revision(source)
    if group not in COMPONENTS:
        raise ValueError("Unknown release group")
    if not re.fullmatch(r"[0-9]{4}-[0-9]{2}-[0-9]{2}", date):
        raise ValueError("Use YYYY-MM-DD for the release date")
    datetime.date.fromisoformat(date)
    ledger = repo.ledger(source)
    attempts = [p for p in ledger["attempts"] if p["group"] == group]
    dead = abandoned(ledger)
    pending = [p for p in attempts if p["id"] not in dead and p["id"] not in completed]
    if pending:
        raise ValueError(
            "Complete or recover the pending release before preparing another"
        )
    public = [p for p in attempts if p["id"] in completed]
    if not public and bootstrap is None:
        return None  # Installation is intentionally inert.
    if public and bootstrap is not None:
        raise ValueError("This group is already activated")
    if bootstrap:
        base_tag, target = bootstrap
        base_sha = repo.revision(base_tag)
        if not repo.ancestor(base_sha, source):
            raise ValueError("Bootstrap baseline is outside main history")
        allowed = r"(?:neo-v|v)?" if group == "platform" else r"installer-v"
        prefix = re.fullmatch(allowed + r"(0\.[0-9]+\.[0-9]+)", base_tag)
        if not prefix:
            raise ValueError(
                "Bootstrap baseline must be an explicit existing version tag"
            )
        if not repo.git("tag", "--list", base_tag):
            raise ValueError("Bootstrap baseline is not a tag")
        base_version = prefix[1]
        version(base_version)
        version(target)
        if version(target) <= version(base_version):
            raise ValueError("Bootstrap must advance the reviewed baseline")
    else:
        previous = max(public, key=lambda p: version(p["version"]))
        base_version, base_tag = previous["version"], release_tag(previous)
        base_sha = completed[previous["id"]]
        if not repo.ancestor(base_sha, source):
            raise ValueError("Published release is outside source history")
    notes = [n for n in repo.fragments(source) if n["group"] == group]
    notes = note_origins(repo, source, notes, ledger)
    notes.sort(
        key=lambda n: (int(repo.git("rev-list", "--count", n["commit"])), n["path"])
    )
    impact = max((n["impact"] for n in notes), key=IMPACTS.get, default="none")
    if bootstrap is None:
        for sha in repo.commits(base_sha, source):
            message = repo.git("show", "-s", "--format=%B", sha)
            level = commit_impact(message)
            if level == "none":
                continue
            associated = [
                n
                for n in repo.fragments(sha)
                if n["path"]
                in repo.git(
                    "diff-tree", "--no-commit-id", "--name-only", "-r", sha
                ).splitlines()
            ]
            if not associated:
                raise ValueError(f"{sha}: releasable commit needs a release fragment")
            groups = {n["group"] for n in associated}
            if (
                level == "breaking"
                and len(groups) != 1
                and not any(n["impact"] == "breaking" for n in associated)
            ):
                raise ValueError("Ambiguous cross-group breaking marker")
            if (
                group in groups
                and len(groups) == 1
                and IMPACTS[level] > IMPACTS[impact]
            ):
                impact = level
    if impact == "none":
        if bootstrap:
            raise ValueError(
                "Manual bootstrap needs reviewed user-facing release notes"
            )
        return None
    if not any(n["impact"] != "none" for n in notes):
        raise ValueError("Releasable commit requires user-facing release notes")
    if impact == "breaking" and not any(n["impact"] == "breaking" for n in notes):
        raise ValueError("Breaking commit requires breaking migration notes")
    reserved = max([base_version] + [p["version"] for p in attempts], key=version)
    if bootstrap is None:
        target = bump(reserved, impact)
    elif version(target) <= version(reserved):
        raise ValueError("Bootstrap version was already reserved")
    plan = identify(
        {
            "kind": "release",
            "group": group,
            "source_sha": source,
            "base_sha": base_sha,
            "base_version": base_version,
            "base_tag": base_tag,
            "version": target,
            "date": date,
            "bootstrap": bool(bootstrap),
            "impact": impact,
            "notes": notes,
        }
    )
    for tag in [release_tag(plan)] + (
        [CONFIG["groups"][group]["alias"] + target]
        if "alias" in CONFIG["groups"][group]
        else []
    ):
        if check_tags and repo.git("tag", "--list", tag):
            raise ValueError(f"Version tag already exists: {tag}")
    return plan


def change_version(text, path, package, target):
    if path.endswith(".cabal"):
        result, count = re.subn(
            r"(?mi)^(version:\s*)[^\n]+", lambda m: m[1] + target, text, count=1
        )
    elif path.endswith("Cargo.toml"):
        match = re.search(r"(?ms)^\[package\]\n.*?(?=^\[|\Z)", text)
        if not match:
            raise ValueError(f"{path}: missing package")
        block, count = re.subn(
            r'(?m)^version = "[^"]+"', f'version = "{target}"', match[0], count=1
        )
        result = text[: match.start()] + block + text[match.end() :]
    else:
        pattern = (
            r'(?ms)^\[\[package\]\]\nname = "'
            + re.escape(package)
            + r'"\nversion = "[^"]+"'
        )
        result, count = re.subn(
            pattern,
            '[[package]]\nname = "' + package + '"\nversion = "' + target + '"',
            text,
        )
    if count != 1:
        raise ValueError(f"{path}: cannot uniquely update {package}")
    return result


def append_changelog(old, section, first):
    old = old or ""
    if first and old:
        old = re.sub(
            r"^## \[?Unreleased\]?\s*$", "## Legacy development notes", old, flags=re.M
        )
        old = "## Legacy history\n\n" + old
    elif old.startswith("# Changelog\n\n"):
        old = old[len("# Changelog\n\n") :]
    return "# Changelog\n\n" + section + "\n" + old


def expected_edits(repo, plan):
    check_identity(plan)
    source, group = plan["source_sha"], plan["group"]
    ledger = repo.ledger(source)
    profile = CONFIG["groups"][group]
    if plan["kind"] == "release":
        if plan["id"] in {p["id"] for p in ledger["attempts"]}:
            raise ValueError("Attempt already prepared")
        if any(
            p["group"] == group and p["version"] == plan["version"]
            for p in ledger["attempts"]
        ):
            raise ValueError("Version already reserved")
        version(plan["version"])
        edits = {}
        for path, package in {**profile["packages"], **profile["locks"]}.items():
            old = repo.read(source, path)
            if old is None:
                raise ValueError(f"Missing version input: {path}")
            edits[path] = change_version(old, path, package, plan["version"])
        for note in plan["notes"]:
            actual = fragment(note["path"], repo.read(source, note["path"]) or "")
            if any(note.get(k) != value for k, value in actual.items()):
                raise ValueError("Fragment content or metadata changed after planning")
            edits[note["path"]] = None
        first = not any(p["group"] == group for p in ledger["attempts"])
        edits[profile["changelog"]] = append_changelog(
            repo.read(source, profile["changelog"]), render(plan), first
        )
        ledger["attempts"].append(plan)
    elif plan["kind"] == "recovery":
        original = next(
            (p for p in ledger["attempts"] if p["id"] == plan["attempt"]), None
        )
        if not original or original["group"] != group:
            raise ValueError("Unknown recovery attempt")
        if original["id"] in abandoned(ledger):
            raise ValueError("Attempt already abandoned")
        edits = {}
        for note in original["notes"]:
            if repo.read(source, note["path"]) is not None:
                raise ValueError("Restored fragment would overwrite a file")
            if digest(note["text"]) != note["hash"]:
                raise ValueError("Recorded fragment hash mismatch")
            edits[note["path"]] = note["text"]
        old = repo.read(source, profile["changelog"]) or ""
        section = render(original)
        if old.count(section) != 1:
            raise ValueError("Failed changelog section changed; reconcile manually")
        edits[profile["changelog"]] = old.replace(section + "\n", "", 1)
        ledger["recoveries"].append(plan)
    else:
        raise ValueError("Unknown release manifest kind")
    edits[LEDGER_PATH] = canonical(ledger)
    return edits


def verify_generated(repo, revision, plan):
    revision = repo.revision(revision)
    if repo.git("rev-parse", revision + "^1") != plan["source_sha"]:
        raise ValueError(
            "Stale preparation: main changed before this preparation merged"
        )
    expected = expected_edits(repo, plan)
    before, after = repo.tree(plan["source_sha"]), repo.tree(revision)
    changed = {p for p in before.keys() | after.keys() if before.get(p) != after.get(p)}
    actual_expected = {
        p for p, v in expected.items() if repo.read(plan["source_sha"], p) != v
    }
    if changed != actual_expected:
        raise ValueError(f"Unaccounted generated diff: {changed ^ actual_expected}")
    for path, text in expected.items():
        if text is not None and after[path][0] != before.get(path, ("100644",))[0]:
            raise ValueError("Generated file mode changed: " + path)
        if repo.read(revision, path) != text:
            raise ValueError(f"Generated metadata was altered: {path}")
    return True


def revision_for(repo, main, plan):
    for revision in repo.commits(plan["source_sha"], main):
        ledger = repo.ledger(revision)
        if any(
            p["id"] == plan["id"] for p in ledger["attempts"] + ledger["recoveries"]
        ):
            verify_generated(repo, revision, plan)
            return revision
    raise ValueError("Release preparation has not merged into main")


def recovery_plan(repo, source, attempt, reason):
    if not substantive(reason):
        raise ValueError("Explain why a source fix/recovery is needed")
    ledger = repo.ledger(source)
    original = next((p for p in ledger["attempts"] if p["id"] == attempt), None)
    if not original:
        raise ValueError("Unknown attempt")
    if attempt in abandoned(ledger):
        return None
    return identify(
        {
            "kind": "recovery",
            "source_sha": repo.revision(source),
            "attempt": attempt,
            "group": original["group"],
            "reason": reason,
        }
    )


def write_edits(root, edits):
    root = Path(root).resolve()
    for name in edits:
        path = root / name
        if path.is_symlink() or not path.resolve().is_relative_to(root):
            raise ValueError(f"Unsafe output path {name}")
    for name, text in edits.items():
        path = root / name
        if text is None:
            path.unlink(missing_ok=True)
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)


def verify_plan(repo, plan, completed):
    before = repo.ledger(plan["source_sha"])
    known = {p["id"] for p in before["attempts"]}
    previous = {k: v for k, v in completed.items() if k in known}
    bootstrap = (plan["base_tag"], plan["version"]) if plan["bootstrap"] else None
    wanted = plan_release(
        repo,
        plan["source_sha"],
        plan["group"],
        previous,
        plan["date"],
        bootstrap,
        check_tags=False,
    )
    if wanted != plan:
        raise ValueError(
            "Release manifest is not the deterministic plan for its source"
        )
    return True


def check_checkout(repo, base, pr_message=None):
    """Validate fragments in a PR/worktree; generated PR exceptions require exact derivation."""
    base = repo.revision(base)
    head = repo.revision("HEAD")
    notes = []
    directory = repo.root / ".changes"
    if directory.is_symlink():
        raise ValueError("Fragment directory cannot be a symlink")
    if directory.exists():
        for path in sorted(directory.rglob("*")):
            if path.is_symlink():
                raise ValueError("Fragment symlink rejected")
            if path.is_file():
                notes.append(
                    fragment(str(path.relative_to(repo.root)), path.read_text())
                )
    before, after = repo.ledger(base), repo.ledger(head)
    additions = [
        p
        for p in after["attempts"] + after["recoveries"]
        if p["id"] not in {x["id"] for x in before["attempts"] + before["recoveries"]}
    ]
    if additions:
        if len(additions) != 1:
            raise ValueError(
                "Generated PR must contain one deterministic release operation"
            )
        plan = additions[0]
        if base != plan["source_sha"]:
            raise ValueError("Generated PR must be refreshed against current main")
        verify_generated(repo, head, plan)
        if plan["kind"] == "release":
            # CI reconstructs the declared prior releases; publication additionally
            # proves their public GitHub receipts before accepting that baseline.
            prior = repo.ledger(plan["source_sha"])
            completed = {
                p["id"]: revision_for(repo, plan["source_sha"], p)
                for p in prior["attempts"]
                if p["id"] not in abandoned(prior)
            }
            verify_plan(repo, plan, completed)
        return "verified generated release operation"
    if before != after:
        raise ValueError("A feature PR cannot rewrite release history")
    # PR checkouts can be synthetic merges; inspect their actual commits too.
    revisions = repo.git("rev-list", "--no-merges", base + ".." + head).splitlines()
    messages = [repo.git("show", "-s", "--format=%B", sha) for sha in revisions]
    if pr_message:
        messages.append(pr_message)
    needs = max((commit_impact(m) for m in messages), key=IMPACTS.get, default="none")
    changed = set(repo.git("diff", "--name-only", base + "..." + head).splitlines())
    pending = [
        n for n in notes if n["path"] in changed or n["path"] not in repo.tree(base)
    ]
    if needs != "none" and not any(n["impact"] != "none" for n in pending):
        raise ValueError(
            "Feature/fix/performance PR needs a reviewed .changes release fragment"
        )
    if needs == "breaking" and not any(n["impact"] == "breaking" for n in pending):
        raise ValueError("Breaking PR needs migration instructions and an agent prompt")
    if any(p in changed for p in ("CHANGELOG.md", "installer/CHANGELOG.md")):
        raise ValueError(
            "Feature PRs write .changes fragments; only derived release/recovery PRs edit changelogs"
        )
    return "release fragments valid"
