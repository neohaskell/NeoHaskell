"""Verify the saved static inventory against its reversible source evidence."""
from pathlib import Path
from collections import deque
import base64
import hashlib
import json

root = Path(__file__).resolve().parent
graph = json.loads((root / "graph.json").read_text())
evidence = json.loads((root / "evidence.json").read_text())
files = {base64.b64decode(row["path_b64"]).decode(): row for row in evidence["files"]}
nodes = {node["id"]: node for node in graph["nodes"]}
assert len(nodes) == len(graph["nodes"]), "duplicate node identifiers"
assert graph["source_revision"] == evidence["source_revision"], "revision mismatch"
adjacency = {key: set() for key in nodes}
for node in nodes.values():
    if node["status"] == "resolved":
        path = base64.b64decode(node["source_path_b64"]).decode()
        row = files[path]
        digest = hashlib.sha256(base64.b64decode(row["content_b64"])).hexdigest()
        assert digest == row["sha256"] == node["source_sha256"], "source hash mismatch"
for edge in graph["edges"]:
    assert edge["source"] in nodes and edge["target"] in nodes, "missing endpoint"
    adjacency[edge["source"]].add(edge["target"])
    node = nodes[edge["source"]]
    path = base64.b64decode(node["source_path_b64"]).decode()
    source_lines = base64.b64decode(files[path]["content_b64"]).decode().splitlines()
    assert edge["line"] is not None, "edge lacks source line"
    excerpt = base64.b64decode(edge["evidence_b64"]).decode()
    assert source_lines[edge["line"] - 1] == excerpt, "edge excerpt mismatch"
reached = {graph["entry_node"]}
queue = deque(reached)
while queue:
    for target in adjacency[queue.popleft()]:
        if target not in reached:
            reached.add(target)
            queue.append(target)
assert reached == set(nodes), "unreachable graph nodes"
for node in nodes.values():
    path = node["discovery_path"]
    assert path[0] == graph["entry_node"] and path[-1] == node["id"], "invalid discovery endpoints"
    assert all(right in adjacency[left] for left, right in zip(path, path[1:])), "invalid discovery edge"
assert all("manual_followup" not in row["disposition"] for row in graph["unresolved_path_ledger"]), "unclassified path target"
for field in ["extractor", "finalizer"]:
    assert hashlib.sha256(base64.b64decode(evidence[field + "_source_b64"])).hexdigest() == evidence[field + "_sha256"], "reproduction source mismatch"
print(f"PASS: {len(nodes)} reachable nodes, {len(graph['edges'])} evidence-bearing edges, {len(files)} source files; all hashes, excerpts and discovery paths match.")
