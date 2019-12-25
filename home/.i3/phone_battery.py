import http.client
import json
import socket
import subprocess
import threading
import time
from os.path import expanduser, expandvars
from threading import Thread

from i3pystatus import IntervalModule


bat_status = {"off": "-", "on": "+", "FULL": "↯"}

class PhoneBattery(IntervalModule):
    token = None
    url = "https://hass.thalheim.io"
    hints = dict(markup=True)

    def request(self, path):
        assert self.token
        conn = http.client.HTTPSConnection("hass.thalheim.io")

        headers = {
            "Authorization": f"Bearer {self.token}",
            "content-type": "application/json",
        }
        conn.request("GET", path, headers=headers)
        resp = conn.getresponse().read()
        return json.loads(resp.decode("utf-8"))

    def run(self) -> None:
        if not self.token:
            cmd = ["bw", "get", "password", "home-assistant-token"]
            res = subprocess.run(cmd, check=True, capture_output=True)
            self.token = res.stdout.decode("utf-8")

        phone_state = self.request("/api/states/device_tracker.redmi_note_5")
        charge_state = self.request("/api/states/binary_sensor.redmi_charging")

        level = phone_state["attributes"]["battery_level"]
        if level == 100:
            status = bat_status["FULL"]
        else:
            status = bat_status[charge_state["state"]]
        self.output = dict(full_text=f"{status}{level}%")
