#!/usr/bin/env python3
"""Reproduce the bounded counts from a saved textual module audit."""

import argparse
import json
from pathlib import Path


def load_graph(path):
    data = json.loads(path.read_text())
    modules = data["modules"]
    local_names = set(modules)
    graph = {
        name: sorted({import_name for import_name in entry.get("imports", []) if import_name in local_names})
        for name, entry in modules.items()
    }
    return data, modules, graph


def closure(graph, roots):
    seen = set()
    pending = list(roots)
    while pending:
        name = pending.pop()
        if name in seen:
            continue
        seen.add(name)
        pending.extend(graph[name])
    return seen


def line_total(modules, names):
    return sum(int(modules[name]["lines"]) for name in names)


def tarjan(graph):
    next_index = 0
    stack = []
    on_stack = set()
    indices = {}
    low_links = {}
    components = []

    def visit(name):
        nonlocal next_index
        indices[name] = next_index
        low_links[name] = next_index
        next_index += 1
        stack.append(name)
        on_stack.add(name)

        for dependency in graph[name]:
            if dependency not in indices:
                visit(dependency)
                low_links[name] = min(low_links[name], low_links[dependency])
            elif dependency in on_stack:
                low_links[name] = min(low_links[name], indices[dependency])

        if low_links[name] == indices[name]:
            component = []
            while True:
                member = stack.pop()
                on_stack.remove(member)
                component.append(member)
                if member == name:
                    break
            components.append(sorted(component))

    for name in sorted(graph):
        if name not in indices:
            visit(name)
    return components


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "json_path",
        nargs="?",
        type=Path,
        default=Path(__file__).with_name("module-graph.json"),
    )
    arguments = parser.parse_args()
    data, modules, graph = load_graph(arguments.json_path)

    core_roots = ["Array", "Text", "Task", "Var", "Environment"]
    required_roots = ["Test.Spec", *core_roots]
    missing = [name for name in required_roots if name not in modules]
    if missing:
        raise SystemExit(f"missing expected roots: {', '.join(missing)}")

    test_spec = closure(graph, ["Test.Spec"])
    core_union = closure(graph, core_roots)
    testlib_inventory = {
        name for name, entry in modules.items() if entry.get("path", "").startswith("core/testlib/")
    }
    components = tarjan(graph)
    nontrivial = [component for component in components if len(component) > 1]

    print(f"source revision: {data.get('revision', 'unknown')}")
    print(f"Test.Spec closure: {len(test_spec)} modules, {line_total(modules, test_spec)} lines")
    print(
        "core-root union closure (Array, Text, Task, Var, Environment): "
        f"{len(core_union)} modules, {line_total(modules, core_union)} lines"
    )
    print(
        f"testlib inventory (core/testlib/): {len(testlib_inventory)} modules, "
        f"{line_total(modules, testlib_inventory)} lines"
    )
    print(f"Tarjan SCCs: {len(components)} total, {len(nontrivial)} non-trivial")
    for component in sorted(nontrivial):
        print("  - " + ", ".join(component))


if __name__ == "__main__":
    main()
