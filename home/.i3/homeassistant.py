import http.client
import json
import socket
import subprocess
import threading
import time
from os.path import expanduser, expandvars
from threading import Thread
from typing import Any, Dict, Optional

from bitwarden import BitwardenPassword
from i3pystatus import IntervalModule
from i3pystatus.core.util import internet, require
from icons import WEATHER_ICONS



def request(path: str,
            token: str,
            data: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
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
    def __init__(self, password: BitwardenPassword):
        self.entities: Optional[Dict[str, Any]] = None
        self.password = password

    def update(self):
        resp = request("/api/states", self.password.get())
        entities = {}
        for entity in resp:
            entities[entity["entity_id"]] = entity
        self.entities = entities

    def get(self, entity_id: str) -> Optional[Dict[str, Any]]:
        if self.entities is None:
            self.update()
        assert self.entities is not None
        return self.entities.get(entity_id)


state = State(BitwardenPassword("home-assistant-token"))


class WeatherIcon(IntervalModule):
    @require(internet)
    def run(self) -> None:
        global state
        weather = state.get("sensor.dark_sky_summary")
        if weather is None:
            return
        icon_name = weather['attributes']['icon']
        icon = WEATHER_ICONS.get(icon_name, None)
        if icon is None:
            text = f'<span color="white">{icon_name}</span>'
        else:
            text = f'<span font_size="xx-large" color="white">{icon}</span>'
        self.output = dict(full_text=text,
                           markup="pango",
                           separator=False,
                           separator_block_width=5)


class WeatherText(IntervalModule):
    @require(internet)
    def run(self) -> None:
        global state
        temperature = state.get("sensor.dark_sky_temperature")
        if temperature is None:
            return
        unit = temperature["attributes"]["unit_of_measurement"]
        text = f'{temperature["state"]}{unit}'

        self.output = dict(full_text=text)


class Shannan(IntervalModule):
    @require(internet)
    def run(self) -> None:
        global state
        shannan = state.get("person.shannan_lekwati")
        distance = state.get("sensor.shannan_joerg_distance")
        locations = {
            "not_home": "Away",
            "Work of Shannan": "Work",
            "Shannan's Home": "Home",
        }
        if shannan is None or distance is None:
            return
        location = locations.get(shannan['state'], shannan['state'])
        self.output = dict(full_text=f"{location} ({distance['state']}km)")


class BikeBattery(IntervalModule):
    entity_id = "input_number.days_bike_light_uncharged"
    on_rightclick = "reset_counter"

    def reset_counter(self) -> None:
        request(f"/api/states/{self.entity_id}",
                state.password.get(),
                data=dict(state=0))
        state.update()
        self.run()

    @require(internet)
    def run(self) -> None:
        global state
        days_uncharged = state.get(self.entity_id)
        if days_uncharged is None:
            return
        days = int(float(days_uncharged["state"]))
        self.output = dict(full_text=f"{days}d")


status_symbols = {"off": "-", "on": "+", "FULL": "↯"}


def format_charge_state(level: int, state: str):
    if level == 100:
        status = status_symbols["FULL"]
    elif state in ["discharging", "NotCharging"]:
        status = status_symbols["off"]
    else:
        status = status_symbols["on"]
    return f"{status}{level}%"


def charge_state_android(state: State, device: str) -> str:
    battery_level = state.get(f"sensor.{device}_battery_level")
    battery_state = state.get(f"sensor.{device}_battery_state")

    if battery_level is None or battery_state is None:
        return "N/A"
    return format_charge_state(battery_level["state"], battery_state["state"])


def charge_state_ios(state: State, device: str) -> str:
    battery = state.get(f"sensor.{device}_battery_state")

    if battery is None:
        return "N/A"
    return format_charge_state(battery["attributes"]["battery"],
                               battery["attributes"]["battery_status"])


class PhoneBattery(IntervalModule):
    hints = dict(markup=True)

    @require(internet)
    def run(self) -> None:
        state.update()
        redmi = charge_state_android(state, "redmi_note_5")
        iphone = charge_state_ios(state, "beatrice")
        watch = charge_state_ios(state, "shannans_apple_watch")

        full_text = f"{redmi} I:{iphone} W:{watch}"

        self.output = dict(full_text=full_text)
