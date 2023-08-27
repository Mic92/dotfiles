import socket
import time
from os.path import expanduser, expandvars
from threading import Thread

from i3pystatus import Module


class YubiKeyTouchDetector(Module):
    socket_path = "$XDG_RUNTIME_DIR/yubikey-touch-detector.socket"
    color = "#00cd66"
    hints = dict(markup=True)

    def init(self):
        self._socket_path = expandvars(expanduser(self.socket_path))
        self.output = dict(
            full_text="",
            color=self.color,
        )
        t = Thread(target=self._run)
        t.daemon = True
        t.start()
        self.requests = set()

    def _connect_socket(self):
        try:
            self.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.socket.connect(self._socket_path)
        except Exception:
            self.socket = None
            self.output = {
                "full_text": "Cannot connect to yubikey-touch-detector"
            }

    def update_status(self):
        if len(self.requests) > 0:
            full_text = '<span font_size="xx-large"></span>'
        else:
            full_text = ""

        self.output = dict(
            full_text=full_text,
            color=self.color
        )

    def _run(self):
        while True:
            self._connect_socket()

            if self.socket is None:
                # Socket is not available, try again soon
                time.sleep(60)
                continue

            while True:
                data = self.socket.recv(5)
                if not data:
                    # Connection dropped, need to reconnect
                    break
                elif data == b"GPG_1":
                    self.requests.add("gpg")
                elif data == b"GPG_0":
                    self.requests.remove("gpg")
                elif data == b"U2F_1":
                    self.requests.add("u2f")
                elif data == b"U2F_0":
                    self.requests.remove("u2f")
                self.update_status()
