#!/usr/bin/env python3
"""Sort herdr tabs into workspaces named after their project directory.

The workspace for a tab is the one labelled like herdr-sesh would name the
active pane's cwd (origin repo name, else git toplevel basename, else cwd
basename), created on demand. Tab labels set by herdr-autoname are not carried
over so autoname keeps managing the moved tab under its new id.
"""

import json
import os
import re
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


def git(cwd: str, *args: str) -> str | None:
    proc = subprocess.run(
        ["git", "-C", cwd, *args], capture_output=True, text=True, check=False
    )
    out = proc.stdout.strip()
    return out if proc.returncode == 0 and out else None


def repo_name(remote: str) -> str:
    """Repository name of a git remote URL, as herdr-sesh derives it."""
    remote = remote.removesuffix(".git")
    if ":" in remote and "://" not in remote:
        remote = remote.rsplit(":", 1)[1]
    name = remote.rstrip("/").rsplit("/", 1)[-1]
    return re.sub(r"[^A-Za-z0-9._/-]+", "-", name)


def project_name(cwd: str) -> str:
    """Workspace label for a directory; must match herdr-sesh's Namer."""
    top = git(cwd, "rev-parse", "--show-toplevel")
    if top is None:
        return Path(cwd).name
    remote = git(cwd, "config", "--get", "remote.origin.url")
    return repo_name(remote) if remote else Path(top).name


def autoname_state() -> dict[str, str]:
    """tab_id -> label last set by herdr-autoname."""
    base = os.environ.get("XDG_STATE_HOME") or str(Path.home() / ".local/state")
    try:
        text = (Path(base) / "herdr-autoname/tabs").read_text()
    except OSError:
        return {}
    return dict(line.split("\t", 1) for line in text.splitlines() if "\t" in line)


def user_label(tab_id: str, label: str, autonamed: dict[str, str]) -> str | None:
    """The label if the user chose it, None for placeholders/autoname labels."""
    if label.isdigit() or not label or autonamed.get(tab_id) == label:
        return None
    return label


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
    tab_label: str | None
    pane_ids: list[str]  # active pane first
    project: str


def active_pane(panes: list[Pane]) -> Pane:
    return next((p for p in panes if p.focused), panes[0])


def plan(
    tabs: list[dict[str, Any]],
    panes: list[Pane],
    workspace_labels: dict[str, str],
    autonamed: dict[str, str],
) -> list[Move]:
    """Tabs whose project name differs from their workspace's label."""
    moves = []
    for tab in tabs:
        in_tab = [p for p in panes if p.tab_id == tab["tab_id"]]
        if not in_tab:
            continue
        lead = active_pane(in_tab)
        if not lead.cwd:
            continue
        project = project_name(lead.cwd)
        if workspace_labels.get(tab["workspace_id"]) == project:
            continue
        rest = [p.pane_id for p in in_tab if p is not lead]
        label = user_label(tab["tab_id"], tab.get("label", ""), autonamed)
        moves.append(Move(tab["tab_id"], label, [lead.pane_id, *rest], project))
    return moves


def apply(move: Move, by_label: dict[str, str]) -> None:
    lead, *rest = move.pane_ids
    target = by_label.get(move.project)
    if target is None:
        args = ["--new-workspace", "--label", move.project]
        if move.tab_label:
            args += ["--tab-label", move.tab_label]
    else:
        args = ["--new-tab", "--workspace", target]
        if move.tab_label:
            args += ["--label", move.tab_label]
    res = herdr("pane", "move", lead, *args, "--no-focus")["move_result"]
    if target is None:
        by_label[move.project] = res["created_workspace"]["workspace_id"]
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
    moves = sorted(
        plan(tabs, panes, labels, autoname_state()),
        key=lambda m: m.tab_id == current,
    )
    for move in moves:
        try:
            apply(move, by_label)
        except (subprocess.CalledProcessError, KeyError) as err:
            print(f"herdr-autospace: {move.tab_id}: {err}", file=sys.stderr)


if __name__ == "__main__":
    main()
