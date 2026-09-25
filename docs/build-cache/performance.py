#!/usr/bin/env python3
"""Render and validate the append-only PR #899 performance timeline.

Raw stage observations stay in the JSON. A route total sums stages within each
repetition first, then takes the median of those totals. The timeline renderer
connects the measured history for each workload metric; pending or missing
metrics remain unplotted rather than becoming fabricated zeroes.
"""

from __future__ import annotations

import argparse
import json
import math
import statistics
import textwrap
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator
import numpy as np


METRICS = (
    ("fresh_route", "Fresh build + suite tests", "#2563eb"),
    ("edit_route", "Edit + core tests", "#bb640f"),
    ("repeat_build", "Rebuild without edits", "#168176"),
)
MINUTE_ROUTES = {"fresh_route", "edit_route"}


def route_observations(workload: dict, route_name: str) -> list[float]:
    routes = workload.get("routes", {})
    if route_name not in routes:
        raise ValueError(f"{workload['id']} {route_name}: route is missing")
    stage_names = routes[route_name]
    stages = workload.get("stages_s", {})
    missing = [stage_name for stage_name in stage_names if stage_name not in stages]
    if missing:
        raise ValueError(
            f"{workload['id']} {route_name}: missing stages {', '.join(missing)}"
        )
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
    workloads = data.get("workloads", [])
    workload_ids = {workload["id"] for workload in workloads}
    for workload in workloads:
        for stage_name, values in workload.get("stages_s", {}).items():
            if (
                not isinstance(values, list)
                or not values
                or any(
                    not isinstance(value, (int, float))
                    or isinstance(value, bool)
                    or not math.isfinite(value)
                    or value < 0
                    for value in values
                )
            ):
                raise ValueError(f"{workload['id']} {stage_name}: invalid observations")
        for route_name in workload.get("routes", {}):
            route_observations(workload, route_name)
    events = data.get("timeline", [])
    if not events:
        raise ValueError("timeline must contain the historical and pending events")
    event_ids = set()
    for event in events:
        if event["id"] in event_ids:
            raise ValueError(f"duplicate timeline event: {event['id']}")
        event_ids.add(event["id"])
        if event["status"] not in {"measured", "pending"}:
            raise ValueError(f"unknown timeline status: {event['status']}")
        if event["status"] == "measured" and event.get("workload") not in workload_ids:
            raise ValueError(f"measured event has no workload: {event['id']}")
        if event["status"] == "pending" and event.get("workload") is not None:
            raise ValueError(f"pending event has fabricated workload: {event['id']}")
    for run in data.get("ci_runs", []):
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
    workload = workloads[event["workload"]]
    if route_name not in workload.get("routes", {}):
        return None
    return route_observations(workload, route_name)


def format_duration(seconds: float, route_name: str) -> str:
    if route_name == "repeat_build":
        return f"{seconds:.1f}s"
    rounded = int(round(seconds))
    minutes, remainder = divmod(rounded, 60)
    return f"{minutes}m {remainder:02d}s"


def summary_duration(seconds: float, route_name: str) -> str:
    if route_name in MINUTE_ROUTES:
        return f"{seconds / 60.0:.3f}m"
    return f"{seconds:.3f}s"


def platform_caption(data: dict) -> str:
    platforms = sorted({workload["platform"] for workload in data["workloads"]})
    if len(platforms) == 1 and "-" in platforms[0]:
        return platforms[0].rsplit("-", 1)[-1]
    return " / ".join(platforms) if platforms else "Recorded runs"


def tick_label(label: str) -> str:
    return "\n".join(textwrap.fill(line, width=20) for line in label.splitlines())


def render(data: dict, output: Path) -> None:
    events = [event for event in data["timeline"] if event["status"] == "measured"]
    if not events:
        raise ValueError("timeline has no measured events to plot")
    x_values = np.arange(len(events), dtype=float)
    figure, axis = plt.subplots(
        figsize=(11.0, 6.5),
        dpi=150,
    )
    figure.patch.set_facecolor("#ffffff")
    figure.subplots_adjust(left=0.085, right=0.72, top=0.79, bottom=0.27)

    figure.text(
        0.045,
        0.94,
        "How build times changed through the experiment",
        fontsize=18,
        weight="bold",
        color="#182333",
    )
    figure.text(
        0.045,
        0.891,
        f"{platform_caption(data)} · median of 3 runs per measured point · lower is faster",
        fontsize=11,
        color="#526172",
    )

    all_minutes: list[float] = []
    for route_name, label, color in METRICS:
        points: list[tuple[int, float, float]] = []
        for event_index, event in enumerate(events):
            observations = timeline_observations(data, event, route_name)
            if observations is None:
                continue
            median_seconds = statistics.median(observations)
            minutes = median_seconds / 60.0
            all_minutes.append(minutes)
            points.append((event_index, minutes, median_seconds))

        if not points:
            continue

        axis.plot(
            [point[0] for point in points],
            [point[1] for point in points],
            "-o",
            color=color,
            linewidth=2.7,
            markersize=7,
            zorder=3,
        )
        for event_index, minutes, seconds in points:
            axis.annotate(
                format_duration(seconds, route_name),
                (event_index, minutes),
                xytext=(0, 12 if route_name != "repeat_build" else 14),
                textcoords="offset points",
                ha="center",
                va="bottom",
                fontsize=10.5,
                color=color,
                weight="bold",
            )

        last_index, last_minutes, _ = points[-1]
        axis.annotate(
            label,
            (last_index, last_minutes),
            xytext=(25, 0),
            textcoords="offset points",
            ha="left",
            va="center",
            fontsize=11.5,
            color=color,
            weight="bold",
            annotation_clip=False,
        )

    maximum_minutes = max(all_minutes, default=1.0)
    axis.set_ylim(-0.4, max(1.0, maximum_minutes * 1.2))
    axis.yaxis.set_major_locator(MultipleLocator(2 if maximum_minutes >= 4 else 1))
    axis.set_ylabel("Elapsed time (minutes)", fontsize=11, labelpad=10, color="#526172")
    axis.set_xlim(-0.1, max(len(events) - 1, 0) + 0.13)
    axis.set_xticks(x_values)
    tick_labels = [tick_label(event["label"]) for event in events]
    axis.set_xticklabels(tick_labels, fontsize=8.7)
    axis.set_xlabel("Experiment sequence →", fontsize=11, labelpad=16, color="#526172")
    axis.tick_params(axis="x", length=0, pad=14)
    axis.tick_params(axis="y", length=0, labelsize=10, labelcolor="#526172")
    axis.grid(axis="y", color="#e5eaf0", linewidth=0.8)
    axis.set_axisbelow(True)
    for spine in axis.spines.values():
        spine.set_visible(False)

    pending_events = [event for event in data["timeline"] if event["status"] == "pending"]
    if pending_events:
        figure.text(
            0.045,
            0.095,
            data.get("pending_footer", "Awaiting rerun"),
            fontsize=10.5,
            color="#526172",
        )
    figure.text(
        0.045,
        0.045,
        data.get(
            "comparison_caveat",
            "Observed sequence; flags and cache conditions differ, so lines describe history rather than causal changes.",
        ),
        fontsize=9.5,
        color="#657386",
    )
    output.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(output, dpi=150, facecolor="white")
    plt.close(figure)


def main() -> None:
    parser = argparse.ArgumentParser(description="compact performance JSON")
    parser.add_argument("--data", type=Path, required=True, help="compact performance JSON")
    parser.add_argument("--output", type=Path, required=True, help="PNG output path")
    args = parser.parse_args()
    data = load_data(args.data)
    render(data, args.output)
    print(f"rendered {args.output}")
    for event in data["timeline"]:
        if event["status"] != "measured":
            continue
        values = []
        for route_name, _, _ in METRICS:
            observations = timeline_observations(data, event, route_name)
            if observations is None:
                values.append(f"{route_name}=unknown")
                continue
            values.append(
                f"{route_name}={summary_duration(statistics.median(observations), route_name)}"
            )
        label = " ".join(event["label"].split())
        print(f"{event['id']} {label}: {', '.join(values)}")


if __name__ == "__main__":
    main()
