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

bat_status = {"off": "-", "on": "+", "FULL": "↯"}


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

    def get(self, entity_id: str) -> Dict[str, Any]:
        if self.entities is None:
            self.update()
        assert self.entities is not None
        return self.entities[entity_id]


state = State(BitwardenPassword("home-assistant-token"))


class WeatherIcon(IntervalModule):
    @require(internet)
    def run(self) -> None:
        global state
        weather = state.get("sensor.dark_sky_summary")
        icon_name = weather['attributes']['icon']
        icon = WEATHER_ICONS.get(icon_name, icon_name)
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
        unit = temperature["attributes"]["unit_of_measurement"]
        text = f'{temperature["state"]}{unit}'

        self.output = dict(full_text=text)


class Shannan(IntervalModule):
    @require(internet)
    def run(self) -> None:
        global state
        shannan = state.get("person.shannan_lekwati")
        self.output = dict(full_text=f"{shannan['state']}")


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
        days = int(float(days_uncharged["state"]))
        self.output = dict(full_text=f"{days}d")


class PhoneBattery(IntervalModule):
    url = "https://hass.thalheim.io"
    hints = dict(markup=True)

    @require(internet)
    def run(self) -> None:
        state.update()
        phone_state = state.get("device_tracker.redmi_note_5")
        charge_state = state.get("binary_sensor.redmi_charging")

        level = phone_state["attributes"]["battery_level"]
        if level == 100:
            status = bat_status["FULL"]
        else:
            status = bat_status[charge_state["state"]]
        self.output = dict(full_text=f"{status}{level}%")
