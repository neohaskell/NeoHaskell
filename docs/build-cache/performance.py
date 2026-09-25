#!/usr/bin/env python3
"""Render and validate the PR #899 performance log.

The input keeps stage observations, rather than stage medians.  A route total
is therefore calculated as the sum of stages at the same repetition index,
then its median is calculated.  This prevents a sum of stage medians from
being reported as a total median.
"""

from __future__ import annotations

import argparse
import json
import statistics
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np


ROUTE_LABELS = {
    "fresh_execution": "fresh execution",
    "fresh_route": "fresh route",
    "edit_route": "representative edit",
}

WORKLOAD_COLORS = {
    "baseline": "#6b7280",
    "candidate": "#2563eb",
    "pilot": "#c47a16",
}


def route_observations(workload: dict, route_name: str) -> list[float]:
    stage_names = workload["routes"][route_name]
    stages = workload["stages_s"]
    observations = [stages[name] for name in stage_names]
    lengths = {len(values) for values in observations}
    if len(lengths) != 1:
        raise ValueError(f"{workload['id']} {route_name}: stage lengths differ")
    count = lengths.pop()
    if count == 0:
        raise ValueError(f"{workload['id']} {route_name}: no observations")
    return [sum(values[index] for values in observations) for index in range(count)]


def validate(data: dict) -> None:
    if data.get("schema") != 1:
        raise ValueError("unsupported performance data schema")
    if data.get("comparability", {}).get("status") != "diagnostic/unmatched":
        raise ValueError("the chart must retain the diagnostic/unmatched guard")
    for workload in data["workloads"]:
        for stage_name, values in workload["stages_s"].items():
            if not values or any(value < 0 for value in values):
                raise ValueError(f"{workload['id']} {stage_name}: invalid observations")
        for route_name in workload["routes"]:
            route_observations(workload, route_name)
    for run in data["ci_runs"]:
        if run["n"] != 1:
            raise ValueError(f"CI row {run['run']} is no longer the explicit n=1 record")
        if run["critical_span_s"] <= 0 or run["runner_sum_s"] <= 0:
            raise ValueError(f"CI row {run['run']}: invalid duration")


def load_data(path: Path) -> dict:
    data = json.loads(path.read_text())
    validate(data)
    return data


def render(data: dict, output: Path) -> None:
    workloads = data["workloads"]
    figure, (sequential, hosted) = plt.subplots(
        1,
        2,
        figsize=(11.0, 6.4),
        dpi=100,
        gridspec_kw={"width_ratios": [1.35, 1.0]},
    )
    figure.patch.set_facecolor("#ffffff")

    # The first panel deliberately has no connecting lines: these are
    # unmatched diagnostic routes, not a time series or an improvement trend.
    route_names = list(ROUTE_LABELS)
    offsets = {"baseline": -0.22, "candidate": 0.0, "pilot": 0.22}
    y_positions = np.arange(len(route_names), dtype=float)
    for workload in workloads:
        color = WORKLOAD_COLORS[workload["id"]]
        for route_index, route_name in enumerate(route_names):
            observations = route_observations(workload, route_name)
            y = y_positions[route_index] + offsets[workload["id"]]
            sequential.scatter(
                observations,
                [y] * len(observations),
                s=34,
                color=color,
                alpha=0.78,
                edgecolors="#ffffff",
                linewidths=0.7,
                zorder=3,
            )
            sequential.plot(
                statistics.median(observations),
                y,
                marker="D",
                markersize=6.0,
                color=color,
                markeredgecolor="#111827",
                markeredgewidth=0.55,
                zorder=4,
            )

    sequential.set_yticks(y_positions)
    sequential.set_yticklabels([ROUTE_LABELS[name] for name in route_names])
    sequential.invert_yaxis()
    sequential.set_xlabel("seconds")
    sequential.set_title("Sequential workload (n=3 each)\nmedian ◆, observations •", loc="left", fontsize=11, pad=10)
    sequential.grid(axis="x", color="#d1d5db", linewidth=0.7, alpha=0.75)
    sequential.set_axisbelow(True)
    sequential.spines[["top", "right", "left"]].set_visible(False)
    sequential.tick_params(axis="y", length=0)
    sequential.set_xlim(left=0)
    maximum = max(
        value
        for workload in workloads
        for route_name in route_names
        for value in route_observations(workload, route_name)
    )
    sequential.set_xlim(right=maximum * 1.20)
    sequential.text(
        0.0,
        -0.16,
        "Each route sums stages within a repetition, then takes the median.\n"
        "Flags and routes are unmatched: diagnostic evidence only.",
        transform=sequential.transAxes,
        fontsize=8.5,
        color="#4b5563",
        va="top",
    )

    # CI spans and runner sums answer different questions and are drawn as
    # separate bars.  The labels carry n=1 because these are not medians.
    ci_runs = data["ci_runs"]
    ci_labels = [
        "candidate macOS\n08ecae9 · n=1",
        "candidate Linux\nc65700c · n=1",
        "candidate-unsplit\n987fe3d · n=1",
    ]
    ci_y = np.arange(len(ci_runs), dtype=float)
    bar_height = 0.31
    critical = [run["critical_span_s"] for run in ci_runs]
    runner = [run["runner_sum_s"] for run in ci_runs]
    hosted.barh(
        ci_y - bar_height / 2,
        critical,
        height=bar_height,
        color="#374151",
        label="critical path span",
        zorder=2,
    )
    hosted.barh(
        ci_y + bar_height / 2,
        runner,
        height=bar_height,
        color="#9ca3af",
        label="runner-time sum",
        zorder=2,
    )
    hosted.set_xlim(left=0, right=max(runner) * 1.46)
    right_label_x = hosted.get_xlim()[1] - 12
    for index, run in enumerate(ci_runs):
        hosted.text(
            right_label_x,
            ci_y[index] + 0.25,
            f"ready→consumer {run['artifact_to_consumer_s']}s",
            ha="right",
            va="center",
            fontsize=8.1,
            color="#374151",
        )
        hosted.text(
            run["critical_span_s"] + 8,
            ci_y[index] - bar_height / 2,
            f"{run['critical_span_s']}s",
            va="center",
            fontsize=8,
            color="#111827",
        )
        hosted.text(
            run["runner_sum_s"] + 8,
            ci_y[index] + bar_height / 2,
            f"{run['runner_sum_s']}s",
            va="center",
            fontsize=8,
            color="#374151",
        )
    hosted.set_yticks(ci_y)
    hosted.set_yticklabels(ci_labels)
    hosted.invert_yaxis()
    hosted.set_xlabel("seconds")
    hosted.set_title("Hosted CI (n=1)\ndark: critical elapsed\nlight: summed runner time", loc="left", fontsize=10.5, pad=10)
    hosted.grid(axis="x", color="#d1d5db", linewidth=0.7, alpha=0.75)
    hosted.set_axisbelow(True)
    hosted.spines[["top", "right", "left"]].set_visible(False)
    hosted.tick_params(axis="y", length=0)
    hosted.text(
        0.0,
        -0.16,
        "Single-run, noncomparable. 987fe3d production\n"
        "is cache-rate-limited/disabled; split jobs excluded.",
        transform=hosted.transAxes,
        fontsize=8.5,
        color="#4b5563",
        va="top",
    )

    figure.suptitle(
        "PR #899 build-cache performance log — diagnostic evidence only",
        fontsize=14,
        fontweight="bold",
        x=0.04,
        ha="left",
        color="#111827",
    )
    figure.text(
        0.04,
        0.015,
        "No speedup claim: compiler flags, split-sections/HIE settings, cache state, and CI job graphs are not yet matched.",
        fontsize=8.6,
        color="#7f1d1d",
    )
    figure.legend(
        handles=[
            Patch(facecolor=WORKLOAD_COLORS["baseline"], label="baseline"),
            Patch(facecolor=WORKLOAD_COLORS["candidate"], label="candidate"),
            Patch(facecolor=WORKLOAD_COLORS["pilot"], label="pilot"),
            Line2D([0], [0], marker="D", color="#111827", markerfacecolor="#ffffff", label="median", linestyle="None", markersize=5.5),
        ],
        loc="upper right",
        bbox_to_anchor=(0.98, 0.995),
        frameon=False,
        ncol=2,
        fontsize=8.4,
    )
    figure.subplots_adjust(left=0.16, right=0.98, top=0.84, bottom=0.22, wspace=0.31)
    output.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(output, dpi=100, facecolor="white")
    plt.close(figure)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, required=True, help="compact performance JSON")
    parser.add_argument("--output", type=Path, required=True, help="PNG output path")
    args = parser.parse_args()
    data = load_data(args.data)
    render(data, args.output)
    print(f"rendered {args.output}")
    for workload in data["workloads"]:
        summaries = []
        for route_name in ("fresh_execution", "fresh_route", "edit_route"):
            observations = route_observations(workload, route_name)
            summaries.append(f"{route_name}={statistics.median(observations):.3f}s")
        print(f"{workload['id']}: {', '.join(summaries)}")


if __name__ == "__main__":
    main()
