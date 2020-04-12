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
        days = int(float(days_uncharged["state"]))
        self.output = dict(full_text=f"{days}d")


class PhoneBattery(IntervalModule):
    hints = dict(markup=True)

    @require(internet)
    def run(self) -> None:
        state.update()
        phone_state = state.get("device_tracker.redmi_note_5")
        charge_state = state.get("binary_sensor.redmi_charging")
        iphone_state = state.get("sensor.beatrice_battery_state")
        watch_state = state.get("device_tracker.shannans_apple_watch")

        level = phone_state["attributes"]["battery_level"]
        if level == 100:
            status = bat_status["FULL"]
        else:
            status = bat_status[charge_state["state"]]

        full_text = f"{status}{level}%"

        iphone_level = iphone_state["attributes"].get("battery", None)
        if iphone_level:
            if level == 100:
                iphone_status = bat_status["FULL"]
            elif iphone_state["attributes"]["battery_status"] == "NotCharging":
                iphone_status = bat_status["off"]
            else:
                iphone_status = bat_status["on"]
            full_text += f" I:{iphone_status}{iphone_level}%"

        watch_level = watch_state["attributes"].get("battery", None)
        if watch_level:
            if level == 100:
                watch_status = bat_status["FULL"]
            elif watch_state["attributes"]["battery_status"] == "NotCharging":
                watch_status = bat_status["off"]
            else:
                watch_status = bat_status["on"]
            full_text += f" W:{watch_status}{watch_level}%"

        self.output = dict(full_text=full_text)
