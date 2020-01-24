from i3pystatus import Status

WEATHER_ICONS = {
    "mdi:weather-cloudy": "摒",
    "mdi:weather-fog": "敖",
    "mdi:weather-hail": "晴",
    "mdi:weather-lightning": "朗",
    "mdi:weather-lightning-rainy": "ﭼ",
    "mdi:weather-night": "望",
    "mdi:weather-partly-cloudy": "杖",
    "mdi:weather-pouring": "歹",
    "mdi:weather-rainy": "殺",
    "mdi:weather-snowy": "流",
    "mdi:weather-snowy-rainy": "ﭽ",
    "mdi:weather-sunny": "滛",
    "mdi:weather-sunset": "滋",
    "mdi:weather-sunset-down": "漢",
    "mdi:weather-sunset-up": "瀞",
    "mdi:weather-windy": "煮",
    "mdi:weather-windy-variant": "瞧",
}


def nerdfont(status: Status, text: str, size: str = "xx-large") -> None:
    status.register(
        "text",
        text=f'<span font_size="{size}" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False,
               "separator_block_width": 5},
    )


def fontawesome(status: Status, text: str, size: str = "large") -> None:
    status.register(
        "text",
        text=f'<span font="FontAwesome" font_size="{size}" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False,
               "separator_block_width": 5},
    )


def conkysymbol(status: Status, text: str) -> None:
    status.register(
        "text",
        text=f'<span font="ConkySymbols" font_size="large" color="white">{text}</span>',
        hints={"markup": "pango", "separator": False,
               "separator_block_width": 5},
    )
