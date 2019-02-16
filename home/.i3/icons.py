from i3pystatus import Status


def nerdfont(status: Status, text: str) -> None:
    status.register(
        "text",
        text=f'<span font_size="xx-large" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )

def fontawesome(status: Status, text: str) -> None:
    status.register(
        "text",
        text=f'<span font="FontAwesome" font_size="large" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )

def conkysymbol(status: Status, text: str) -> None:
    status.register(
        "text",
        text=f'<span font="ConkySymbols" font_size="large" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False, "separator_block_width": 5},
    )
