"""QR code and barcode detection using pyzbar (zbar)."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class CodeBox:
    """A detected QR code or barcode with bounding box and decoded value."""

    data: str
    code_type: str  # e.g. "QRCODE", "EAN13", "CODE128"
    x: int
    y: int
    width: int
    height: int

    @property
    def x2(self) -> int:
        return self.x + self.width

    @property
    def y2(self) -> int:
        return self.y + self.height

    def contains_point(self, px: float, py: float) -> bool:
        return self.x <= px <= self.x2 and self.y <= py <= self.y2

    def intersects_rect(self, rx: int, ry: int, rw: int, rh: int) -> bool:
        return not (
            self.x2 < rx or self.x > rx + rw or self.y2 < ry or self.y > ry + rh
        )


def scan_codes(image_path: Path) -> list[CodeBox]:
    """Scan an image for QR codes and barcodes.

    Uses pyzbar (zbar) to detect and decode all barcodes and QR codes
    in the image.  Returns a list of CodeBox with decoded data and
    bounding rectangles.
    """
    from PIL import Image  # lazy import
    from pyzbar.pyzbar import decode  # lazy import

    img = Image.open(image_path)
    results = decode(img)

    codes: list[CodeBox] = []
    for result in results:
        data = result.data.decode("utf-8", errors="replace")
        code_type = result.type
        rect = result.rect
        codes.append(
            CodeBox(
                data=data,
                code_type=code_type,
                x=rect.left,
                y=rect.top,
                width=rect.width,
                height=rect.height,
            )
        )

    return codes
