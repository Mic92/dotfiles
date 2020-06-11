from threading import Thread
from typing import Any
from enum import Enum
import json

from i3pystatus import Module
import paho.mqtt.client as mqtt
from i3pystatus.core.desktop import DesktopNotification


class RhasspyState(Enum):
    disconnected = 0
    connected = 1
    in_session = 2


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


class Rhasspy(Module):
    host = "localhost"
    port = 12183
    password = None
    color = "#FFFFFF"
    settings = (
        "host",
        "port",
        "password",
        "color"
    )

    def on_connect(self, client: mqtt.Client, userdata: Any, flags: int, rc: int) -> None:
        client.subscribe(Topic.SESSION_STARTED)
        client.subscribe(Topic.TEXT_CAPTURED)
        client.subscribe(Topic.INTENT_PARSED)
        client.subscribe(Topic.INTENT_NOT_RECOGNIZED)
        client.subscribe(Topic.TTS_SAY)
        client.subscribe(Topic.SESSION_ENDED)

        self.state = RhasspyState.connected

    @property
    def state(self):
        return self._state

    @state.setter
    def state(self, value):
        self._state = value
        self.update_status()

    def on_disconnect(self, client: mqtt.Client, userdata: Any, rc: int) -> None:
        self.state = RhasspyState.disconnected

    def notify(self, text, title="Rhasspy:"):
        DesktopNotification(title=title, body=text).display()

    def on_message(self, client: mqtt.Client, userdata: Any, msg: mqtt.MQTTMessage) -> None:
        payload = json.loads(msg.payload)

        if msg.topic == Topic.SESSION_STARTED:
            self.state = RhasspyState.in_session
        elif msg.topic == Topic.SESSION_ENDED:
            self.state = RhasspyState.connected
        elif msg.topic == Topic.TEXT_CAPTURED:
            self.notify(payload["text"], title="You:")
        elif msg.topic == Topic.INTENT_PARSED:
            self.notify(f"Intent {payload['intentName']} detected.")
        elif msg.topic == Topic.INTENT_NOT_RECOGNIZED:
            self.notify(f"Intent not recognized.")
        elif msg.topic == Topic.TTS_SAY:
            self.notify(payload["text"])

    def init(self) -> None:
        self.output = dict(
            full_text="disconnected",
            color=self.color,
        )
        self.client = mqtt.Client(self.host, self.port)
        self.client.connect(self.host, self.port, 60)
        self.client.on_connect = self.on_connect
        self.client.on_disconnect = self.on_disconnect
        self.client.on_message = self.on_message
        t = Thread(target=self._run)
        t.daemon = True
        t.start()
        self.state = RhasspyState.disconnected

    def update_status(self) -> None:
        if self.state == RhasspyState.in_session:
            full_text = "in session"
        elif self.state == RhasspyState.connected:
            full_text = "connected"
        elif self.state == RhasspyState.disconnected:
            full_text = "disconnected"
        else:
            full_text = "invalid state"
        self.output = dict(
            full_text=full_text,
            color=self.color
        )

    def _run(self):
        self.client.loop_forever()
