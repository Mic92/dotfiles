"""Tests for QR code and barcode detection."""

from __future__ import annotations

from pathlib import Path

import pytest

from live_text.barcode import CodeBox, scan_codes


def _code(
    data: str = "https://example.com",
    code_type: str = "QRCODE",
    x: int = 10,
    y: int = 20,
    width: int = 100,
    height: int = 100,
) -> CodeBox:
    """Create a CodeBox with sensible defaults for tests."""
    return CodeBox(data=data, code_type=code_type, x=x, y=y, width=width, height=height)


class TestCodeBox:
    def test_contains_point(self) -> None:
        c = _code(x=10, y=20, width=100, height=80)
        assert c.contains_point(50, 50)  # inside
        assert c.contains_point(10, 20)  # top-left edge
        assert c.contains_point(110, 100)  # bottom-right edge
        assert not c.contains_point(5, 50)  # left of
        assert not c.contains_point(50, 105)  # below

    def test_intersects_rect(self) -> None:
        c = _code(x=10, y=20, width=100, height=80)
        assert c.intersects_rect(0, 0, 200, 200)  # containing
        assert c.intersects_rect(50, 50, 10, 10)  # inside code
        assert c.intersects_rect(100, 90, 30, 30)  # partial overlap
        assert not c.intersects_rect(120, 110, 50, 50)  # no overlap
        assert not c.intersects_rect(0, 0, 5, 5)  # completely left/above


def _has_pyzbar() -> bool:
    try:
        from pyzbar.pyzbar import decode  # noqa: F401

        return True
    except ImportError:
        return False


def _has_pillow() -> bool:
    try:
        from PIL import Image  # noqa: F401

        return True
    except ImportError:
        return False


@pytest.mark.skipif(
    not (_has_pyzbar() and _has_pillow()), reason="pyzbar or Pillow not installed"
)
class TestScanCodes:
    def test_no_codes_on_blank_image(self, tmp_path: Path) -> None:
        """A blank white image should produce no codes."""
        from PIL import Image

        blank = Image.new("RGB", (200, 200), "white")
        blank_path = tmp_path / "blank.png"
        blank.save(str(blank_path))

        codes = scan_codes(blank_path)
        assert codes == []

    def test_detects_qr_code(self, tmp_path: Path) -> None:
        """Generate a QR code image and verify it's detected with correct data."""
        qrcode = pytest.importorskip("qrcode")

        qr = qrcode.QRCode(version=1, box_size=10, border=4)
        qr.add_data("https://example.com/test")
        qr.make(fit=True)
        img = qr.make_image(fill_color="black", back_color="white")
        qr_path = tmp_path / "qr.png"
        img.save(str(qr_path))

        codes = scan_codes(qr_path)
        assert len(codes) == 1
        assert codes[0].data == "https://example.com/test"
        assert codes[0].code_type == "QRCODE"
        assert codes[0].width > 0
        assert codes[0].height > 0

    def test_detects_barcode(self, tmp_path: Path) -> None:
        """Generate a Code128 barcode and verify detection."""
        barcode_mod = pytest.importorskip("barcode")

        code128 = barcode_mod.get(
            "code128", "TEST12345", writer=barcode_mod.writer.ImageWriter()
        )
        barcode_path = tmp_path / "barcode"
        saved_path = code128.save(str(barcode_path))

        codes = scan_codes(Path(saved_path))
        assert len(codes) >= 1
        assert any("TEST12345" in c.data for c in codes)

    def test_multiple_qr_codes(self, tmp_path: Path) -> None:
        """An image with two QR codes should detect both."""
        qrcode = pytest.importorskip("qrcode")
        from PIL import Image

        qr1 = qrcode.QRCode(version=1, box_size=5, border=2)
        qr1.add_data("CODE_ONE")
        qr1.make(fit=True)
        img1 = qr1.make_image(fill_color="black", back_color="white").get_image()

        qr2 = qrcode.QRCode(version=1, box_size=5, border=2)
        qr2.add_data("CODE_TWO")
        qr2.make(fit=True)
        img2 = qr2.make_image(fill_color="black", back_color="white").get_image()

        # Compose side by side on a white canvas
        w1, h1 = img1.size
        w2, h2 = img2.size
        canvas = Image.new("RGB", (w1 + w2 + 40, max(h1, h2) + 20), "white")
        canvas.paste(img1, (10, 10))
        canvas.paste(img2, (w1 + 30, 10))
        composite_path = tmp_path / "multi_qr.png"
        canvas.save(str(composite_path))

        codes = scan_codes(composite_path)
        decoded_data = {c.data for c in codes}
        assert "CODE_ONE" in decoded_data
        assert "CODE_TWO" in decoded_data

    def test_bounding_boxes_dont_overlap_for_separate_codes(
        self, tmp_path: Path
    ) -> None:
        """Two QR codes placed far apart should have non-overlapping boxes."""
        qrcode = pytest.importorskip("qrcode")
        from PIL import Image

        qr1 = qrcode.QRCode(version=1, box_size=5, border=2)
        qr1.add_data("LEFT")
        qr1.make(fit=True)
        img1 = qr1.make_image(fill_color="black", back_color="white").get_image()

        qr2 = qrcode.QRCode(version=1, box_size=5, border=2)
        qr2.add_data("RIGHT")
        qr2.make(fit=True)
        img2 = qr2.make_image(fill_color="black", back_color="white").get_image()

        w1, h1 = img1.size
        w2, h2 = img2.size
        gap = 200
        canvas = Image.new("RGB", (w1 + w2 + gap, max(h1, h2) + 20), "white")
        canvas.paste(img1, (10, 10))
        canvas.paste(img2, (w1 + gap, 10))
        path = tmp_path / "separated.png"
        canvas.save(str(path))

        codes = scan_codes(path)
        assert len(codes) == 2
        # Boxes should not overlap: one should be entirely left of the other
        left, right = sorted(codes, key=lambda c: c.x)
        assert left.x2 < right.x
