#!/usr/bin/env python3
"""Sort herdr tabs into workspaces named after their project directory.

A tab's project is the git toplevel of its active pane's cwd (or the cwd
itself outside git). The workspace for a project is the one labelled with the
project's basename -- the naming herdr-sesh uses -- and is created on demand.
"""

import json
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


def herdr(*args: str) -> dict[str, Any]:
    out = subprocess.run(
        ["herdr", *args], check=True, capture_output=True, text=True
    ).stdout
    result: dict[str, Any] = json.loads(out)["result"]
    return result


def project_dir(cwd: str) -> Path:
    proc = subprocess.run(
        ["git", "-C", cwd, "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
        check=False,
    )
    top = proc.stdout.strip()
    return Path(top) if proc.returncode == 0 and top else Path(cwd)


@dataclass
class Pane:
    pane_id: str
    tab_id: str
    workspace_id: str
    cwd: str
    focused: bool


@dataclass
class Move:
    tab_id: str
    tab_label: str
    pane_ids: list[str]  # active pane first
    project: Path


def active_pane(panes: list[Pane]) -> Pane:
    return next((p for p in panes if p.focused), panes[0])


def plan(
    tabs: list[dict[str, Any]],
    panes: list[Pane],
    workspace_labels: dict[str, str],
) -> list[Move]:
    """Tabs whose project basename differs from their workspace's label."""
    moves = []
    for tab in tabs:
        in_tab = [p for p in panes if p.tab_id == tab["tab_id"]]
        if not in_tab:
            continue
        lead = active_pane(in_tab)
        if not lead.cwd:
            continue
        project = project_dir(lead.cwd)
        if workspace_labels.get(tab["workspace_id"]) == project.name:
            continue
        rest = [p.pane_id for p in in_tab if p is not lead]
        moves.append(
            Move(tab["tab_id"], tab.get("label", ""), [lead.pane_id, *rest], project)
        )
    return moves


def apply(move: Move, by_label: dict[str, str]) -> None:
    lead, *rest = move.pane_ids
    target = by_label.get(move.project.name)
    if target is None:
        res = herdr(
            "pane", "move", lead,
            "--new-workspace", "--label", move.project.name,
            "--tab-label", move.tab_label, "--no-focus",
        )["move_result"]  # fmt: skip
        by_label[move.project.name] = res["created_workspace"]["workspace_id"]
    else:
        res = herdr(
            "pane", "move", lead,
            "--new-tab", "--workspace", target,
            "--label", move.tab_label, "--no-focus",
        )["move_result"]  # fmt: skip
    new_tab = res["pane"]["tab_id"]
    # Remaining panes lose their layout but stay together in the tab. Their
    # ids are workspace-scoped and unchanged until they themselves move.
    for pane_id in rest:
        herdr(
            "pane", "move", pane_id, "--tab", new_tab, "--split", "right", "--no-focus"
        )


def main() -> None:
    workspaces = herdr("workspace", "list")["workspaces"]
    tabs = herdr("tab", "list")["tabs"]
    panes = [
        Pane(
            p["pane_id"],
            p["tab_id"],
            p["workspace_id"],
            p.get("foreground_cwd") or p.get("cwd") or "",
            bool(p.get("focused")),
        )
        for p in herdr("pane", "list")["panes"]
    ]
    labels = {w["workspace_id"]: w.get("label", "") for w in workspaces}
    by_label = {label: wid for wid, label in labels.items() if label}
    # Move the focused tab last so focus stays where the user is looking.
    current = os.environ.get("HERDR_TAB_ID")
    moves = sorted(plan(tabs, panes, labels), key=lambda m: m.tab_id == current)
    for move in moves:
        try:
            apply(move, by_label)
        except (subprocess.CalledProcessError, KeyError) as err:
            print(f"herdr-autospace: {move.tab_id}: {err}", file=sys.stderr)


if __name__ == "__main__":
    main()
