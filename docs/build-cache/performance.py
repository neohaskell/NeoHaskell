#!/usr/bin/env python3
"""Render and validate the append-only PR #899 performance timeline.

Raw stage observations stay in the JSON. A route total sums stages within each
repetition first, then takes the median of those totals. The timeline renderer
does not connect unmatched configurations; a colored line appears only after
two measured points share a ``line_group``.
"""

from __future__ import annotations

import argparse
import json
import statistics
from collections import defaultdict
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np


METRICS = (
    ("fresh_route", "Fresh build + test", "minutes", 60.0),
    ("edit_route", "Implementation edit + core test", "minutes", 60.0),
    ("repeat_build", "Repeat build", "seconds", 1.0),
)

SERIES = {
    "cabal": {"label": "Cabal", "color": "#6b7280"},
    "nix-unsplit": {"label": "Nix unsplit", "color": "#2563eb"},
    "extracted-pilot": {"label": "Extracted pilot", "color": "#c47a16"},
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
    workload_ids = {workload["id"] for workload in data["workloads"]}
    for workload in data["workloads"]:
        for stage_name, values in workload["stages_s"].items():
            if not values or any(value < 0 for value in values):
                raise ValueError(f"{workload['id']} {stage_name}: invalid observations")
        for route_name in workload["routes"]:
            route_observations(workload, route_name)
    if not data.get("timeline"):
        raise ValueError("timeline must contain the historical and pending events")
    for event in data["timeline"]:
        if event["series"] not in SERIES:
            raise ValueError(f"unknown timeline series: {event['series']}")
        if event["status"] == "measured" and event["workload"] not in workload_ids:
            raise ValueError(f"measured event has no workload: {event['id']}")
        if event["status"] == "pending" and event["workload"] is not None:
            raise ValueError(f"pending event has fabricated workload: {event['id']}")
    for run in data["ci_runs"]:
        if run["n"] != 1:
            raise ValueError(f"CI row {run['run']} is no longer the explicit n=1 record")
        if run["critical_span_s"] <= 0 or run["runner_sum_s"] <= 0:
            raise ValueError(f"CI row {run['run']}: invalid duration")


def load_data(path: Path) -> dict:
    data = json.loads(path.read_text())
    validate(data)
    return data


def timeline_observations(data: dict, event: dict, route_name: str) -> list[float] | None:
    if event["status"] != "measured":
        return None
    workloads = {workload["id"]: workload for workload in data["workloads"]}
    return route_observations(workloads[event["workload"]], route_name)


def render(data: dict, output: Path) -> None:
    events = data["timeline"]
    x_values = np.arange(len(events), dtype=float)
    figure, axes = plt.subplots(
        len(METRICS),
        1,
        figsize=(11.0, 7.4),
        dpi=100,
        sharex=True,
    )
    figure.patch.set_facecolor("#ffffff")

    for axis, (route_name, title, unit, divisor) in zip(axes, METRICS):
        measured_by_group: dict[tuple[str, str], list[tuple[float, float]]] = defaultdict(list)
        all_values: list[float] = []
        for event_index, event in enumerate(events):
            observations = timeline_observations(data, event, route_name)
            if observations is None:
                axis.axvline(event_index, color="#d1d5db", linestyle=":", linewidth=0.8, zorder=0)
                continue
            display_observations = [value / divisor for value in observations]
            display_median = statistics.median(display_observations)
            all_values.extend(display_observations)
            color = SERIES[event["series"]]["color"]
            # Three small dots preserve replicated observations without
            # suggesting that unmatched configurations form a trend.
            jitter = np.linspace(-0.055, 0.055, len(display_observations))
            axis.scatter(
                [event_index + value for value in jitter],
                display_observations,
                s=24,
                color=color,
                alpha=0.72,
                edgecolors="#ffffff",
                linewidths=0.6,
                zorder=3,
            )
            axis.plot(
                event_index,
                display_median,
                marker="D",
                markersize=6.0,
                color=color,
                markeredgecolor="#111827",
                markeredgewidth=0.55,
                zorder=4,
            )
            if event.get("line_group"):
                measured_by_group[(event["series"], event["line_group"])].append(
                    (event_index, display_median)
                )
            axis.annotate(
                f"{display_median:.1f}{unit[0]}",
                xy=(event_index, display_median),
                xytext=(0, 8),
                textcoords="offset points",
                ha="center",
                va="bottom",
                fontsize=8.7,
                color="#111827",
            )

        for (series_name, _), points in measured_by_group.items():
            if len(points) >= 2:
                points.sort()
                axis.plot(
                    [point[0] for point in points],
                    [point[1] for point in points],
                    color=SERIES[series_name]["color"],
                    linewidth=1.8,
                    alpha=0.9,
                    zorder=2,
                )

        axis.set_ylabel(unit)
        axis.set_title(f"{title} · lower is better", loc="left", fontsize=11, pad=8)
        axis.grid(axis="y", color="#d1d5db", linewidth=0.7, alpha=0.75)
        axis.set_axisbelow(True)
        axis.spines[["top", "right", "left"]].set_visible(False)
        axis.tick_params(axis="y", length=0)
        axis.set_xlim(-0.45, len(events) - 0.55)
        if all_values:
            axis.set_ylim(bottom=0, top=max(all_values) * 1.28)

    axes[-1].set_xticks(x_values)
    tick_labels = [
        f"{event['id']}\n{event['label']}\n{event['commit']}"
        + ("\nawaiting measurements" if event["status"] == "pending" else "")
        for event in events
    ]
    axes[-1].set_xticklabels(tick_labels, fontsize=8.4)
    for tick, event in zip(axes[-1].get_xticklabels(), events):
        if event["status"] == "pending":
            tick.set_color("#6b7280")
    axes[-1].set_xlabel("Chronological experiment changes; pending entries have no invented value")

    figure.suptitle(
        "Build performance over experiment revisions",
        fontsize=14,
        fontweight="bold",
        x=0.04,
        ha="left",
        color="#111827",
    )
    figure.text(
        0.04,
        0.015,
        "Diagnostic only: each method has one measured configuration (n=3); unmatched revisions remain unconnected until matched reruns land.",
        fontsize=8.5,
        color="#7f1d1d",
    )
    figure.legend(
        handles=[
            Patch(facecolor=SERIES["cabal"]["color"], label="Cabal"),
            Patch(facecolor=SERIES["nix-unsplit"]["color"], label="Nix unsplit"),
            Patch(facecolor=SERIES["extracted-pilot"]["color"], label="Extracted pilot"),
            Line2D([0], [0], marker="D", color="#111827", markerfacecolor="#ffffff", label="median", linestyle="None", markersize=5.5),
        ],
        loc="upper right",
        bbox_to_anchor=(0.98, 0.995),
        frameon=False,
        ncol=2,
        fontsize=8.4,
    )
    figure.subplots_adjust(left=0.11, right=0.98, top=0.88, bottom=0.22, hspace=0.52)
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
    for event in data["timeline"]:
        if event["status"] == "measured":
            values = []
            for route_name, _, _, divisor in METRICS:
                observations = timeline_observations(data, event, route_name)
                values.append(f"{route_name}={statistics.median(observations) / divisor:.3f}{'m' if divisor == 60.0 else 's'}")
            print(f"{event['id']} {event['label']}: {', '.join(values)}")


if __name__ == "__main__":
    main()
