#!/usr/bin/env python3

import base64
import io
import json
import os
import re
import socket
import ssl
import sys
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler
from pathlib import Path
from urllib.parse import urlparse

DEBUG = os.environ.get("DEBUG") is not None


@dataclass
class IRCConfig:
    """IRC connection configuration."""

    server: str
    nick: str
    channel: str
    sasl_password: str | None = None
    server_password: str | None = None
    tls: bool = True
    port: int = 6697


def _create_socket(tls: bool) -> socket.socket:
    """Create and configure socket."""
    sock = socket.socket(family=socket.AF_INET6)
    if tls:
        context = ssl.create_default_context()
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE
        sock = context.wrap_socket(sock)
    return sock


def _authenticate(sock: socket.socket, config: IRCConfig) -> None:
    """Handle authentication."""

    def _send(command: str) -> int:
        if DEBUG:
            print(command)
        return sock.send((f"{command}\r\n").encode())

    if config.server_password:
        _send(f"PASS {config.server_password}")

    if config.sasl_password:
        _send("CAP REQ :sasl")
        _send("AUTHENTICATE PLAIN")
        auth = base64.encodebytes(
            f"{config.nick}\0{config.nick}\0{config.sasl_password}".encode()
        )
        _send(f"AUTHENTICATE {auth.decode('ascii')}")
        _send("CAP END")

    _send(f"USER {config.nick} 0 * :{config.nick}")
    _send(f"NICK {config.nick}")


def _wait_for_response(recv_file: io.TextIOWrapper, sock: socket.socket) -> None:
    """Wait for initial server response."""

    def _pong(ping: str) -> None:
        if ping.startswith("PING"):
            sock.send(ping.replace("PING", "PONG").encode("ascii"))

    for line in recv_file.readline():
        if re.match(r"^:[^ ]* (MODE|221|376|422) ", line):
            break
        _pong(line)


def _send_messages(sock: socket.socket, channel: str, messages: list[str]) -> None:
    """Send messages to channel."""

    def _send(command: str) -> int:
        if DEBUG:
            print(command)
        return sock.send((f"{command}\r\n").encode())

    _send(f"JOIN :{channel}")
    for m in messages:
        _send(f"PRIVMSG {channel} :{m}")


def _wait_for_completion(recv_file: io.TextIOWrapper, sock: socket.socket) -> None:
    """Wait for server to acknowledge messages."""

    def _pong(ping: str) -> None:
        if ping.startswith("PING"):
            sock.send(ping.replace("PING", "PONG").encode("ascii"))

    def _send(command: str) -> int:
        if DEBUG:
            print(command)
        return sock.send((f"{command}\r\n").encode())

    _send("INFO")
    for line in recv_file:
        if DEBUG:
            print(line, end="")
        if "End of /INFO" in line:
            break
        _pong(line)


def _irc_send(config: IRCConfig, messages: list[str]) -> None:
    if not messages:
        return

    sock = _create_socket(config.tls)
    recv_file = sock.makefile(mode="r")

    print(f"connect {config.server}:{config.port}")
    sock.connect((config.server, config.port))

    _authenticate(sock, config)
    _wait_for_response(recv_file, sock)
    _send_messages(sock, config.channel, messages)
    _wait_for_completion(recv_file, sock)

    sock.send(b"QUIT")
    print("disconnect")
    sock.close()


def irc_send(
    url: str,
    notifications: list[str],
    password: str | None = None,
) -> None:
    parsed = urlparse(f"{url}")
    username = parsed.username or "prometheus"
    server = parsed.hostname or "chat.freenode.net"
    channel = f"#{parsed.fragment}" if parsed.fragment != "" else "#krebs-announce"
    port = parsed.port or 6697
    if not password:
        password = parsed.password
    if len(notifications) == 0:
        return
    config = IRCConfig(
        server=server,
        nick=username,
        channel=channel,
        sasl_password=password,
        server_password=None,
        tls=parsed.scheme == "irc+tls",
        port=port,
    )
    _irc_send(config, notifications)


class PrometheusWebHook(BaseHTTPRequestHandler):
    def __init__(
        self,
        irc_url: str,
        conn: socket.socket,
        addr: tuple[str, int],
        password: str | None = None,
    ) -> None:
        self.irc_url = irc_url
        self.password = password
        self.rfile = conn.makefile("rb")
        self.wfile = conn.makefile("wb")
        self.client_address = addr
        self.handle()

    # for testing
    def do_GET(self) -> None:
        self.send_response(200)
        self.send_header("Content-type", "text/plain")
        self.end_headers()
        self.wfile.write(b"ok")

    def do_POST(self) -> None:
        content_type = self.headers.get("content-type", "")
        # Parse content type header, splitting on semicolon to ignore parameters
        content_type = content_type.split(";")[0].strip()

        # refuse to receive non-json content
        if content_type != "application/json":
            self.send_response(400)
            self.end_headers()
            return
        length = int(self.headers.get("content-length", 0))
        payload = json.loads(self.rfile.read(length))
        messages = []
        for alert in payload["alerts"]:
            description = alert["annotations"]["description"]
            messages.append(f"{alert['status']}: {description}")
        irc_send(self.irc_url, messages, password=self.password)

        self.do_GET()


def systemd_socket_response() -> None:
    irc_url = os.environ.get("IRC_URL", None)
    if irc_url is None:
        print(
            "IRC_URL environment variable not set: i.e. IRC_URL=irc+tls://mic92-prometheus@chat.freenode.net/#krebs-announce",
            file=sys.stderr,
        )
        sys.exit(1)

    password = None
    irc_password_file = os.environ.get("IRC_PASSWORD_FILE", None)
    if irc_password_file:
        password = Path(irc_password_file).read_text().strip()

    msgs = sys.argv[1:]

    if msgs != []:
        irc_send(irc_url, msgs, password=password)
        return

    nfds = os.environ.get("LISTEN_FDS", None)
    if nfds is None:
        print(
            "LISTEN_FDS not set. Run me with systemd(TM) socket activation?",
            file=sys.stderr,
        )
        sys.exit(1)
    fds = range(3, 3 + int(nfds))

    for fd in fds:
        sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(0)

        try:
            while True:
                PrometheusWebHook(irc_url, *sock.accept(), password=password)
        except BlockingIOError:
            # no more connections
            pass


if __name__ == "__main__":
    if len(sys.argv) == 3:
        print(f"{sys.argv[1]} {sys.argv[2]}")
        irc_send(sys.argv[1], [sys.argv[2]])
    else:
        systemd_socket_response()
