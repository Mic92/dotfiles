"""CLI utilities and helper functions."""

from typing import Any


def print_table(headers: list[str], rows: list[list[Any]]) -> None:
    """Print a simple table."""
    # Calculate column widths
    col_widths = [len(str(h)) for h in headers]
    for row in rows:
        for i, cell in enumerate(row):
            col_widths[i] = max(col_widths[i], len(str(cell)))

    # Print header
    header_line = " | ".join(str(h).ljust(w) for h, w in zip(headers, col_widths, strict=False))
    print(header_line)
    print("-" * len(header_line))

    # Print rows
    for row in rows:
        row_line = " | ".join(str(cell).ljust(w) for cell, w in zip(row, col_widths, strict=False))
        print(row_line)
