import contextlib
import http.client
import json
import time
from threading import Thread
from typing import Any

import color
from bitwarden import BitwardenPassword
from i3pystatus import IntervalModule


def request(path: str,
            token: str,
            data: dict[str, Any] | None = None) -> dict[str, Any]:
    conn = http.client.HTTPSConnection("hass.thalheim.io")

    headers = {
        "Authorization": f"Bearer {token}",
        "content-type": "application/json",
    }
    method = "GET"
    body = None
    if data:
        body = json.dumps(data)
        method = "POST"
    conn.request(method, path, headers=headers, body=body)
    resp = conn.getresponse().read()
    return json.loads(resp.decode("utf-8"))


class State:
    def __init__(self, password: BitwardenPassword) -> None:
        self.entities: dict[str, Any] | None = None
        self.password = password
        thread = Thread(target=self.update_state)
        thread.daemon = True
        thread.start()

    def update_state(self):
        while True:
            with contextlib.suppress(Exception):
                self.update()
            time.sleep(30)

    def update(self):
        resp = request("/api/states", self.password.get())
        entities = {}
        for entity in resp:
            entities[entity["entity_id"]] = entity
        self.entities = entities

    def get(self, entity_id: str) -> dict[str, Any] | None:
        while self.entities is None:
            time.sleep(1)
        return self.entities.get(entity_id)


state = State(BitwardenPassword("home-assistant-token"))


# class WeatherIcon(IntervalModule):
#    @require(internet)
#    def run(self) -> None:
#        global state
#        weather = state.get("weather.openweathermap")
#        if weather is None:
#            return
#        text = f"{weather['attributes']['temperature']}°C"
#        #icon_name = weather['attributes']['icon']
#        #icon = WEATHER_ICONS.get(icon_name, None)
#        #if icon is None:
#        #    text = f'<span color="white">{icon_name}</span>'
#        #else:
#        #    text = f'<span font_size="xx-large" color="white">{icon}</span>'
#        self.output = dict(full_text=text,
#                           markup="pango"
#                           separator=False,
#                           separator_block_width=5)


class WeatherText(IntervalModule):
    def run(self) -> None:
        weather = state.get("weather.openweathermap")
        if weather is None:
            return

        air_quality = state.get("sensor.bme680_air_quality")
        temperature = state.get("sensor.bme680_temperature")

        text = f"{weather['attributes']['temperature']}°C (inside: {temperature['state']}°C/air: {air_quality['state']}%)"

        self.output = dict(full_text=text)


class Shannan(IntervalModule):
    def run(self) -> None:
        shannan = state.get("person.shannan_lekwati")
        distance = state.get("sensor.shannan_joerg_distance")
        locations = {
            "not_home": "Away",
            "Work of Shannan": "Work",
            "Shannan's Home": "Home",
        }
        if shannan is None or distance is None:
            return
        location = locations.get(shannan["state"], shannan["state"])
        self.output = dict(full_text=f"{location} ({distance['state']}km)")


status_symbols = {"off": "-", "on": "+", "FULL": "↯"}


def charge_state(level: int, state: str) -> tuple[str, bool]:
    if level == 100:
        status = status_symbols["FULL"]
    elif state in ["discharging", "NotCharging"]:
        status = status_symbols["off"]
    else:
        status = status_symbols["on"]
    return f"{status}{level}%", level < 40


def charge_state_android(state: State, device: str) -> tuple[str, bool]:
    battery_level = state.get(f"sensor.{device}_battery_level")
    battery_state = state.get(f"sensor.{device}_battery_state")

    if battery_level == "unknown" or battery_level is None or battery_state is None:
        return "N/A", False
    return charge_state(int(battery_level["state"]), battery_state["state"])


def charge_state_ios(state: State, device: str) -> tuple[str, bool]:
    battery = state.get(f"sensor.{device}_battery_state")

    if battery is None or "battery" not in battery["attributes"]:
        return "N/A", False
    return charge_state(battery["attributes"]["battery"],
                        battery["attributes"]["battery_status"])


class PhoneBattery(IntervalModule):
    def run(self) -> None:
        redmi = charge_state_android(state, "android")
        iphone = charge_state_ios(state, "beatrice_icloud")
        watch = charge_state_ios(state, "shannans_apple_watch")
        full_text = f"{redmi[0]} I:{iphone[0]} W:{watch[0]}"
        critical = redmi[1] or iphone[1] or watch[1]
        output = dict(full_text=full_text)
        if critical:
            output["color"] = color.text_down

        self.output = output
