import json
import os
import subprocess
import time
from enum import Enum
from threading import Thread
from typing import Any

import paho.mqtt.client as mqtt
from i3pystatus import Module
from i3pystatus.core.desktop import DesktopNotification


class RhasspyState(Enum):
    disconnected = 0
    connected = 1
    listening = 2


class Topic:
    # 1. fired when start listening
    SESSION_STARTED = "hermes/dialogueManager/sessionStarted"
    # 2. fired when stop listening
    TEXT_CAPTURED = "hermes/asr/textCaptured"
    # 3.1 Intent recognized
    INTENT_PARSED = "hermes/asr/intentParsed"
    # 3.2 no intent recognized
    INTENT_NOT_RECOGNIZED = "hermes/nlu/intentNotRecognized"
    # 4. Text to say, might be from an intent, but can be also send via IP.
    TTS_SAY = "hermes/tts/say"
    # 5. After intent is handled
    SESSION_ENDED = "hermes/dialogueManager/sessionEnded"
    # 6. After tts ended
    TTS_ENDED = "hermes/tts/sayFinished"


class Rhasspy(Module):
    host = "localhost"
    port = 12183
    color_connected = "#FFFFFF"
    color_disconnected = "#FF0000"
    color_listening = "#00FF00"
    format_connected = "◉"
    format_listening = "◉"
    format_disconnected = "N/A"
    settings = (
        ("host", "Hostname/ip address of the MQTT broker connect to it."),
        ("port", "Port number of the MQTT broker connect"),
        ("color_connected", "Text color"),
        ("format_connected", "Text color"),
        ("color_disconnected", "Text color"),
        ("format_disconnected", "Text color"),
        ("color_listening", "Text color"),
        ("format_listening", "Text color"),
    )

    def on_connect(self,
                   client: mqtt.Client,
                   userdata: Any,
                   flags: int,
                   rc: int) -> None:
        client.subscribe(Topic.SESSION_STARTED)
        client.subscribe(Topic.TEXT_CAPTURED)
        client.subscribe(Topic.INTENT_PARSED)
        client.subscribe(Topic.INTENT_NOT_RECOGNIZED)
        client.subscribe(Topic.TTS_SAY)
        client.subscribe(Topic.SESSION_ENDED)
        client.subscribe(Topic.TTS_ENDED)

        self.update_state(RhasspyState.connected)

    def on_disconnect(self, client: mqtt.Client, userdata: Any, rc: int) -> None:
        self.state = RhasspyState.disconnected

    def notify(self, text: str, title: str="Rhasspy:") -> None:
        DesktopNotification(title=title, body=text).display()

    def on_message(self, client: mqtt.Client, userdata: Any, msg: mqtt.MQTTMessage) -> None:
        payload = json.loads(msg.payload)

        if msg.topic == Topic.SESSION_STARTED:
            self.notify("Listening.")
            self.update_state(RhasspyState.listening)
        elif msg.topic == Topic.SESSION_ENDED:
            self.update_state(RhasspyState.connected)
        elif msg.topic == Topic.TEXT_CAPTURED:
            self.update_state(RhasspyState.connected)
            self.notify(payload["text"], title="You:")
        elif msg.topic == Topic.INTENT_PARSED:
            self.notify(f"Intent {payload['intentName']} detected.")
        elif msg.topic == Topic.INTENT_NOT_RECOGNIZED:
            self.notify("Intent not recognized.")
        elif msg.topic == Topic.TTS_SAY:
            self.notify(payload["text"])
        elif msg.topic == Topic.TTS_ENDED:
            if "neutral-janet" in payload["sessionId"]:
                subprocess.Popen(["paplay", os.path.expanduser("~/.config/rhasspy/profiles/en/end-of-conversation.wav")])

    def init(self) -> None:
        self.output = dict(
            full_text="disconnected",
            color=self.color_disconnected,
        )
        t = Thread(target=self._run)
        t.daemon = True
        t.start()
        self.update_state(RhasspyState.disconnected)

    def update_state(self, state: RhasspyState) -> None:
        if state == RhasspyState.listening:
            full_text = self.format_listening
            color = self.color_listening
        elif state == RhasspyState.connected:
            full_text = self.format_connected
            color = self.color_connected
        elif state == RhasspyState.disconnected:
            full_text = self.format_disconnected
            color = self.color_disconnected
        else:
            msg = "invalid state: {self.state}"
            raise RuntimeError(msg)
        self.output = dict(
            full_text=full_text,
            color=color
        )

    def _run(self):
        while True:
            try:
                self.client = mqtt.Client(self.host, self.port)
                break
            except Exception:
                time.sleep(3)

        self.client.connect(self.host, self.port, 60)
        self.client.on_connect = self.on_connect
        self.client.on_disconnect = self.on_disconnect
        self.client.on_message = self.on_message
        self.client.loop_forever()
