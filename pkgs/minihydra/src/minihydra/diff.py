"""Compute high-level diff between base and head sides of a run."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass
class DiffEntry:
    attr: str
    base: dict[str, Any] | None
    head: dict[str, Any] | None


@dataclass
class Diff:
    added: list[DiffEntry] = field(default_factory=list)
    removed: list[DiffEntry] = field(default_factory=list)
    changed: list[DiffEntry] = field(default_factory=list)
    unchanged: list[DiffEntry] = field(default_factory=list)
    newly_broken: list[DiffEntry] = field(default_factory=list)
    newly_fixed: list[DiffEntry] = field(default_factory=list)
    still_broken: list[DiffEntry] = field(default_factory=list)

    def summary(self) -> dict[str, int]:
        return {
            "added": len(self.added),
            "removed": len(self.removed),
            "changed": len(self.changed),
            "unchanged": len(self.unchanged),
            "newly_broken": len(self.newly_broken),
            "newly_fixed": len(self.newly_fixed),
            "still_broken": len(self.still_broken),
        }


def _row_to_dict(row: Any) -> dict[str, Any]:
    return {k: row[k] for k in row.keys()}  # noqa: SIM118 (sqlite3.Row has no __contains__)


def compute(base_rows: list[Any], head_rows: list[Any]) -> Diff:
    base = {r["attr"]: _row_to_dict(r) for r in base_rows}
    head = {r["attr"]: _row_to_dict(r) for r in head_rows}
    diff = Diff()
    for attr in sorted(set(base) | set(head)):
        b = base.get(attr)
        h = head.get(attr)
        entry = DiffEntry(attr=attr, base=b, head=h)
        b_err = bool(b and b.get("error"))
        h_err = bool(h and h.get("error"))
        if b is None and h is not None:
            (diff.newly_broken if h_err else diff.added).append(entry)
        elif h is None and b is not None:
            diff.removed.append(entry)
        elif b is not None and h is not None:
            if b_err and h_err:
                diff.still_broken.append(entry)
            elif b_err and not h_err:
                diff.newly_fixed.append(entry)
            elif h_err and not b_err:
                diff.newly_broken.append(entry)
            elif b.get("drv_path") != h.get("drv_path"):
                diff.changed.append(entry)
            else:
                diff.unchanged.append(entry)
    return diff
