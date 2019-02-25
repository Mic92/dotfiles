from i3pystatus import Status


def nerdfont(status: Status, text: str, size: str = "xx-large") -> None:
    status.register(
        "text",
        text=f'<span font_size="{size}" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )


def fontawesome(status: Status, text: str, size: str = "large") -> None:
    status.register(
        "text",
        text=f'<span font="FontAwesome" font_size="{size}" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )


def conkysymbol(status: Status, text: str) -> None:
    status.register(
        "text",
        text=f'<span font="ConkySymbols" font_size="large" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )
