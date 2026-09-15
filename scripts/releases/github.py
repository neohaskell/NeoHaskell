"""GitHub effects, isolated from planning and replaceable by fixture backends."""

import json
import io
import zipfile
import os
from pathlib import Path
import urllib.error
import urllib.parse
import urllib.request

import engine as release


class GitHub:
    def __init__(self):
        self.token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
        if not self.token:
            raise ValueError(
                "GitHub operations require the workflow GITHUB_TOKEN or local gh authentication"
            )
        self.prefix = "repos/" + release.REPOSITORY

    def request(self, route, method="GET", data=None, binary=False, upload=False):
        host = "https://uploads.github.com/" if upload else "https://api.github.com/"
        headers = {
            "Authorization": "Bearer " + self.token,
            "Accept": (
                "application/octet-stream" if binary else "application/vnd.github+json"
            ),
            "X-GitHub-Api-Version": "2022-11-28",
            "User-Agent": "NeoHaskell-release",
        }
        if isinstance(data, bytes):
            headers["Content-Type"] = "application/octet-stream"
        elif data is not None:
            data = json.dumps(data).encode()
            headers["Content-Type"] = "application/json"
        request = urllib.request.Request(
            host + self.prefix + "/" + route, data=data, headers=headers, method=method
        )
        try:
            with urllib.request.urlopen(request, timeout=120) as response:
                result = response.read()
        except urllib.error.HTTPError as error:
            if error.code == 404 and method == "GET":
                return None
            raise ValueError(
                f'GitHub {method} {route.split("?")[0]} failed with HTTP {error.code}'
            ) from None
        return result if binary else (json.loads(result) if result else None)

    def ref(self, name):
        data = self.request("git/ref/" + name)
        if not data:
            return None
        obj = data["object"]
        for _ in range(10):
            if obj["type"] == "commit":
                return obj["sha"]
            if obj["type"] != "tag":
                break
            obj = self.request("git/tags/" + obj["sha"])["object"]
        raise ValueError("Git reference does not resolve to a commit")

    def ensure_ref(self, name, sha):
        old = self.ref(name)
        if old is not None:
            if old != sha:
                raise ValueError(f"Conflicting immutable ref {name}")
            return
        self.request("git/refs", "POST", {"ref": "refs/" + name, "sha": sha})

    def paused(self, identity):
        return self.ref("heads/release-pause/" + identity) is not None

    def pause(self, identity, sha):
        if not self.paused(identity):
            self.ensure_ref("heads/release-pause/" + identity, sha)

    def resume(self, identity):
        self.request("git/refs/heads/release-pause/" + identity, "DELETE")

    def get_release(self, tag):
        published = self.request("releases/tags/" + urllib.parse.quote(tag, safe=""))
        if published:
            return published
        # The tag endpoint only promises published releases. Authenticated
        # listings include drafts, which hold the frozen bundle during retries.
        matches = []
        for page in range(1, 100):
            batch = self.request(f"releases?per_page=100&page={page}")
            if not isinstance(batch, list):
                raise ValueError("Cannot list repository releases for draft recovery")
            matches.extend(item for item in batch if item["tag_name"] == tag)
            if len(matches) > 1:
                raise ValueError(f"Ambiguous existing releases for {tag}")
            if len(batch) < 100:
                return matches[0] if matches else None
        raise ValueError("Unexpectedly large release list during draft recovery")

    def create_release(self, tag, sha, title, body):
        return self.request(
            "releases",
            "POST",
            {
                "tag_name": tag,
                "target_commitish": sha,
                "name": title,
                "body": body,
                "draft": True,
                "prerelease": False,
            },
        )

    def assets(self, item):
        result = []
        for page in range(1, 100):
            batch = self.request(
                f'releases/{item["id"]}/assets?per_page=100&page={page}'
            )
            result.extend(batch)
            if len(batch) < 100:
                return result
        raise ValueError("Unexpectedly large release asset set")

    def read_asset(self, asset):
        return self.request(f'releases/assets/{asset["id"]}', binary=True)

    def upload(self, item, name, content):
        return self.request(
            f'releases/{item["id"]}/assets?name=' + urllib.parse.quote(name, safe=""),
            "POST",
            content,
            upload=True,
        )

    def publish(self, item):
        return self.request(
            f'releases/{item["id"]}',
            "PATCH",
            {
                "draft": False,
                "make_latest": "true",
            },
        )

    def make_pr(self, repo, plan, edits):
        # Each immutable source/plan has its own branch. Repeated events reuse it;
        # stale plans remain reviewable but cannot pass provenance verification.
        branch = "release/" + plan["group"] + "-" + plan["id"][:16]
        expected_tree = []
        for path, text in edits.items():
            row = {
                "path": path,
                "mode": repo.tree(plan["source_sha"]).get(path, ("100644",))[0],
                "type": "blob",
            }
            if text is None:
                row["sha"] = None
            else:
                row["content"] = text
            expected_tree.append(row)
        tree = self.request(
            "git/trees",
            "POST",
            {
                "base_tree": repo.git("rev-parse", plan["source_sha"] + "^{tree}"),
                "tree": expected_tree,
            },
        )["sha"]
        old = self.ref("heads/" + branch)
        if old:
            commit = self.request("git/commits/" + old)
            if commit["tree"]["sha"] != tree or [
                p["sha"] for p in commit["parents"]
            ] != [plan["source_sha"]]:
                raise ValueError(
                    "Generated PR branch changed; refusing to overwrite it"
                )
        else:
            title = f"ci(release): {'recover' if plan['kind']=='recovery' else 'prepare'} {plan['group']} {plan.get('version',plan['attempt'][:12] if 'attempt' in plan else '')}"
            commit = self.request(
                "git/commits",
                "POST",
                {"message": title, "tree": tree, "parents": [plan["source_sha"]]},
            )
            self.ensure_ref("heads/" + branch, commit["sha"])
        prs = self.request(
            "pulls?state=open&head="
            + urllib.parse.quote(
                release.REPOSITORY.split("/")[0] + ":" + branch, safe=""
            )
            + "&base=main"
        )
        if prs:
            return prs[0]["html_url"]
        title = f"ci(release): {'recover' if plan['kind']=='recovery' else 'prepare'} {plan['group']} {plan.get('version','release')}"
        body = (
            "This prepares the reviewed release notes and version updates for users. Merging this PR starts verified builds and publication.\n\n"
            if plan["kind"] == "release"
            else "This abandons an unpublished release that cannot ship. Its notes return to the pending set so a replacement can include the fix.\n\n"
        )
        body += (
            "Generated from main commit `"
            + plan["source_sha"]
            + "`. Required CI must pass; GitHub may ask a maintainer to approve Actions runs created by this workflow.\n\n"
            + (release.render(plan) if plan["kind"] == "release" else plan["reason"])
        )
        return self.request(
            "pulls",
            "POST",
            {
                "title": title,
                "body": body,
                "head": branch,
                "base": "main",
                "draft": False,
            },
        )["html_url"]


def tags(plan):
    profile = release.group_config(plan["group"])
    return [release.release_tag(plan)] + (
        [profile["alias"] + plan["version"]] if "alias" in profile else []
    )


def asset_hash(api, asset):
    checksum = asset.get("digest", "") or ""
    if checksum.startswith("sha256:") and len(checksum) == 71:
        return checksum[7:]
    return release.digest(api.read_asset(asset))


def checksum_map(data):
    result = {}
    for line in data.decode().splitlines():
        import re

        match = re.fullmatch(r"([0-9a-f]{64})  ([A-Za-z0-9._-]+)", line)
        if not match or match[2] in result:
            raise ValueError("Invalid/duplicate checksum entry")
        result[match[2]] = match[1]
    return result


def verify_remote(api, repo, plan, item):
    """A public receipt is evidence only after source, notes, tags, and assets agree."""
    if item.get("draft") or item.get("prerelease"):
        raise ValueError("Release is not a completed stable publication")
    assets = api.assets(item)
    by_name = {a["name"]: a for a in assets}
    if (
        len(by_name) != len(assets)
        or "release-manifest.json" not in by_name
        or "SHA256SUMS" not in by_name
    ):
        raise ValueError("Published release lacks an unambiguous manifest/checksum set")
    checks = checksum_map(api.read_asset(by_name["SHA256SUMS"]))
    if set(checks) != set(by_name) - {"SHA256SUMS"}:
        raise ValueError("Published asset set differs from checksums")
    for name, wanted in checks.items():
        if asset_hash(api, by_name[name]) != wanted:
            raise ValueError("Published asset checksum mismatch: " + name)
    receipt = json.loads(api.read_asset(by_name["release-manifest.json"]))
    if receipt["plan"] != plan or item.get("body") != release.render(plan):
        raise ValueError("Published notes/manifest mismatch")
    revision = receipt["revision"]
    release.verify_generated(repo, revision, plan)
    expected_names = required_assets(plan)
    if set(receipt["assets"]) != expected_names or set(checks) != expected_names | {
        "release-manifest.json",
        "release-bundle.zip",
    }:
        raise ValueError(
            "Published release is missing required native artifacts/evidence"
        )
    if any(checks[n] != h for n, h in receipt["assets"].items()):
        raise ValueError("Receipt/checksum mismatch")
    for tag in tags(plan):
        if api.ref("tags/" + tag) != revision:
            raise ValueError("Published tag alias/source mismatch")
    return revision


def completed_releases(api, repo, ledger):
    result = {}
    for plan in ledger["attempts"]:
        if plan["id"] in release.abandoned(ledger):
            item = api.get_release(release.release_tag(plan))
            if item and not item["draft"]:
                raise ValueError("Published release cannot have an abandonment record")
            if not api.paused(plan["id"]):
                raise ValueError("Abandonment is missing its durable pause record")
            continue
        item = api.get_release(release.release_tag(plan))
        if item and not item["draft"]:
            result[plan["id"]] = verify_remote(api, repo, plan, item)
    return result


def required_assets(plan):
    prefix = release.group_config(plan["group"])["asset_prefix"]
    names = {prefix + t for t in release.TARGETS} | {
        "receipt-" + t + ".json" for t in release.TARGETS
    }
    names |= {
        "neo-starter-flake.nix",
        "neo-starter-flake.lock",
        "neo-starter-cabal.project",
        "neo-compatibility.json",
        "consumer.json",
    }
    return names


def bundle_bytes(files):
    stream = io.BytesIO()
    with zipfile.ZipFile(stream, "w", compression=zipfile.ZIP_STORED) as archive:
        for name, content in sorted(files.items()):
            entry = zipfile.ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            entry.external_attr = 0o100644 << 16
            archive.writestr(entry, content)
    return stream.getvalue()


def unpack_bundle(plan, revision, content):
    names = required_assets(plan) | {"release-manifest.json"}
    with zipfile.ZipFile(io.BytesIO(content)) as archive:
        if len(archive.infolist()) != len(names) or set(archive.namelist()) != names:
            raise ValueError("Frozen bundle has unexpected or duplicate files")
        if sum(i.file_size for i in archive.infolist()) > 2_000_000_000:
            raise ValueError("Frozen bundle exceeds the release size limit")
        files = {name: archive.read(name) for name in names}
    receipt = json.loads(files["release-manifest.json"])
    if receipt != {
        "plan": plan,
        "revision": revision,
        "assets": {n: release.digest(files[n]) for n in required_assets(plan)},
    }:
        raise ValueError("Frozen bundle provenance/checksums differ")
    return files


def restore_bundle(api, plan, revision, directory):
    item = api.get_release(release.release_tag(plan))
    if not item:
        return False
    bundles = [a for a in api.assets(item) if a["name"] == "release-bundle.zip"]
    if not bundles:
        return False
    if len(bundles) != 1:
        raise ValueError("Duplicate frozen bundle")
    content = api.read_asset(bundles[0])
    if release.digest(content) != asset_hash(api, bundles[0]):
        raise ValueError("Frozen bundle transport checksum mismatch")
    files = unpack_bundle(plan, revision, content)
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    for name, data in {**files, "release-bundle.zip": content}.items():
        path = directory / name
        if path.is_symlink():
            raise ValueError("Unsafe bundle output")
        path.write_bytes(data)
    return True


def seal_artifacts(plan, revision, directory):
    """Check native gate receipts before sealing the immutable publication inputs."""
    directory = Path(directory)
    names = required_assets(plan)
    actual = {p.name for p in directory.iterdir()}
    if actual - {"release-manifest.json", "SHA256SUMS", "release-bundle.zip"} != names:
        raise ValueError("Missing/unexpected release assets")
    hashes = {}
    for name in names:
        path = directory / name
        if not path.is_file() or path.is_symlink():
            raise ValueError("Release asset is not a regular file")
        hashes[name] = release.digest(path.read_bytes())
    prefix = release.group_config(plan["group"])["asset_prefix"]
    for target in release.TARGETS:
        receipt = json.loads((directory / ("receipt-" + target + ".json")).read_text())
        expected = {
            "attempt": plan["id"],
            "revision": revision,
            "target": target,
            "binary_hash": hashes[prefix + target],
            "group": plan["group"],
            "native_verified": True,
        }
        expected["starter_hashes"] = {
            n: hashes["neo-starter-" + n]
            for n in ("flake.nix", "flake.lock", "cabal.project")
        }
        if receipt != expected:
            raise ValueError("Native build/source/portability receipt mismatch")
    compat = json.loads((directory / "neo-compatibility.json").read_text())
    if (
        compat["neo_version"] != plan["version"]
        or compat["neohaskell"]["source_revision"] != revision
    ):
        raise ValueError("Shipped starter compatibility does not match release source")
    consumer = json.loads((directory / "consumer.json").read_text())
    expected = {
        "attempt": plan["id"],
        "revision": revision,
        "binary_hash": hashes["neo-x86_64-unknown-linux-gnu"],
        "verified": True,
    }
    if consumer != expected:
        raise ValueError("Exact released binary lacks consumer verification")
    receipt = {"plan": plan, "revision": revision, "assets": hashes}
    (directory / "release-manifest.json").write_text(release.canonical(receipt))
    hashes["release-manifest.json"] = release.digest(
        (directory / "release-manifest.json").read_bytes()
    )
    contents = {n: (directory / n).read_bytes() for n in hashes}
    bundle = directory / "release-bundle.zip"
    if bundle.exists():
        if (
            bundle.is_symlink()
            or unpack_bundle(plan, revision, bundle.read_bytes()) != contents
        ):
            raise ValueError("Frozen bundle differs from verified inputs")
    else:
        bundle.write_bytes(bundle_bytes(contents))
    hashes["release-bundle.zip"] = release.digest(bundle.read_bytes())
    (directory / "SHA256SUMS").write_text(
        "".join(f"{h}  {name}\n" for name, h in sorted(hashes.items()))
    )
    return {p.name: p.read_bytes() for p in directory.iterdir()}


def publish(api, repo, plan, revision, files, guard):
    """Resume identical partial uploads; never overwrite an existing asset or ref."""
    release.verify_generated(repo, revision, plan)
    guard()
    if api.paused(plan["id"]):
        raise ValueError("Release is paused for recovery")
    item = api.get_release(release.release_tag(plan))
    if item and not item["draft"]:
        verify_remote(api, repo, plan, item)
        for asset in api.assets(item):
            if asset["name"] not in files or asset_hash(api, asset) != release.digest(
                files[asset["name"]]
            ):
                raise ValueError("Completed release differs from the retry")
        return False
    for tag in tags(plan):
        guard()
        api.ensure_ref("tags/" + tag, revision)
    if item is None:
        guard()
        item = api.create_release(
            release.release_tag(plan),
            revision,
            ("NeoHaskell " if plan["group"] == "platform" else "Installer ")
            + plan["version"],
            release.render(plan),
        )
    if item["body"] != release.render(plan):
        raise ValueError("Existing draft notes differ; refusing overwrite")
    existing = {a["name"]: a for a in api.assets(item)}
    if set(existing) - set(files):
        raise ValueError("Existing draft contains unexpected assets")
    for name, content in sorted(
        files.items(), key=lambda pair: (pair[0] != "release-bundle.zip", pair[0])
    ):
        guard()
        if name in existing:
            if asset_hash(api, existing[name]) != release.digest(content):
                raise ValueError("Existing asset differs: " + name)
        else:
            api.upload(item, name, content)
    guard()
    # Re-read all uploaded bytes before making the draft public.
    actual = api.assets(item)
    if {a["name"] for a in actual} != set(files) or any(
        asset_hash(api, a) != release.digest(files[a["name"]]) for a in actual
    ):
        raise ValueError("Uploaded asset verification failed")
    guard()
    api.publish(item)
    return True


def select_work(
    api,
    repo,
    main,
    operation="auto",
    group="platform",
    baseline=None,
    target=None,
    date=None,
    attempt=None,
    reason=None,
):
    import datetime

    # Same main snapshot always proposes the same date, even on a later retry.
    date = (
        date
        or datetime.datetime.fromtimestamp(
            int(repo.git("show", "-s", "--format=%ct", main)), datetime.timezone.utc
        )
        .date()
        .isoformat()
    )
    ledger = repo.ledger(main)
    if operation == "auto" and not ledger["attempts"]:
        return {
            "status": "inactive — first manual release required",
            "items": [],
            "prs": [],
        }
    completed = completed_releases(api, repo, ledger)
    # A recovery commit is not a trusted signal just because it looks like one.
    for recovery in ledger["recoveries"]:
        release.revision_for(repo, main, recovery)
    if operation in {"recover", "resume"}:
        original = next((p for p in ledger["attempts"] if p["id"] == attempt), None)
        if not original:
            raise ValueError("Unknown release attempt")
        if attempt in completed:
            raise ValueError("Published releases cannot be abandoned or resumed")
        if attempt in release.abandoned(ledger):
            if operation == "resume":
                raise ValueError("An abandoned release cannot resume")
            return {"status": "already abandoned", "items": [], "prs": []}
        release.revision_for(repo, main, original)
        if operation == "resume":
            if api.paused(attempt):
                api.resume(attempt)
            return {"status": "resumed; dispatch auto to retry", "items": [], "prs": []}
        # The workflow holds the same global mutation lock as publication.
        plan = release.recovery_plan(repo, main, attempt, reason or "")
        api.pause(attempt, main)
        return {
            "status": "recovery paused and prepared",
            "items": [],
            "prs": [api.make_pr(repo, plan, release.expected_edits(repo, plan))],
        }
    if operation == "bootstrap":
        if not baseline or not target:
            raise ValueError(
                "Manual bootstrap requires explicit baseline tag and target version"
            )
        plan = release.plan_release(
            repo, main, group, completed, date, (baseline, target)
        )
        return {
            "status": "manual bootstrap preparation",
            "items": [],
            "prs": [api.make_pr(repo, plan, release.expected_edits(repo, plan))],
        }
    items, prs = [], []
    for current in release.COMPONENTS:
        attempts = [p for p in ledger["attempts"] if p["group"] == current]
        pending = [
            p
            for p in attempts
            if p["id"] not in completed and p["id"] not in release.abandoned(ledger)
        ]
        if len(pending) > 1:
            raise ValueError("Multiple pending releases in one group")
        if pending:
            plan = pending[0]
            if api.paused(plan["id"]):
                continue
            release.verify_plan(repo, plan, completed)
            revision = release.revision_for(repo, main, plan)
            item = api.get_release(release.release_tag(plan))
            reuse = bool(
                item
                and any(a["name"] == "release-bundle.zip" for a in api.assets(item))
            )
            items.append(
                {
                    "group": current,
                    "attempt": plan["id"],
                    "revision": revision,
                    "reuse": reuse,
                }
            )
        else:
            plan = release.plan_release(repo, main, current, completed, date)
            if plan:
                prs.append(api.make_pr(repo, plan, release.expected_edits(repo, plan)))
    return {
        "status": "prepared" if prs else ("publish" if items else "idle"),
        "items": items,
        "prs": prs,
    }
