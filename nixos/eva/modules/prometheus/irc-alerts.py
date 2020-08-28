#!/usr/bin/env python3

import socket
import os
import sys
import cgi
import json
import ssl
from http.server import BaseHTTPRequestHandler
from typing import Tuple, Optional, List
from urllib.parse import urlparse


def _irc_send(
    server: str,
    nick: str,
    password: Optional[str],
    channel: str,
    tls: bool = True,
    port: int = 6697,
    messages: List[str] = [],
) -> None:
    if not messages:
        return

    sock = socket.socket()
    if tls:
        sock = ssl.wrap_socket(
            sock, cert_reqs=ssl.CERT_NONE, ssl_version=ssl.PROTOCOL_TLSv1_2
        )

    def _send(command: str) -> int:
        return sock.send((f"{command}\r\n").encode())

    print(f"connect {server}:{port}")
    sock.connect((server, port))
    if password:
        _send(f"PASS {password}")
    _send(f"USER {nick} {server} bla :{nick}")
    _send(f"NICK {nick}")
    _send(f"JOIN :{channel}")

    for m in messages:
        _send(f"PRIVMSG {channel} :{m}")

    _send("INFO")


    while True:
        data = sock.recv(4096)
        if not data:
            raise RuntimeError("Received empty data")

        # Assume INFO reply means we are done
        if b"End of /INFO list" in data:
            break

        if data.startswith(b"PING"):
            sock.send(data.replace(b"PING", b"PONG"))

    sock.send(b"QUIT")
    print("disconnect")
    sock.close()


def irc_send(url: str, notifications: List[str]) -> None:
    parsed = urlparse(f"{url}")
    username = parsed.username or "prometheus"
    server = parsed.hostname or "chat.freenode.de"
    if parsed.fragment != "":
        channel = f"#{parsed.fragment}"
    else:
        channel = "#krebs-announce"
    port = parsed.port or 6697
    password = parsed.password
    if len(notifications) == 0:
        return
    _irc_send(
        server=server,
        nick=username,
        password=password,
        channel=channel,
        port=port,
        messages=notifications,
        tls=parsed.scheme == "irc+tls"
    )


class PrometheusWebHook(BaseHTTPRequestHandler):
    def __init__(self, irc_url: str, conn: socket.socket, addr: Tuple[str, int]) -> None:
        self.irc_url = irc_url
        self.rfile = conn.makefile("rb")
        self.wfile = conn.makefile("wb")
        self.client_address = addr
        self.handle()

    # for testing
    def do_GET(self):
        self.send_response(200)
        self.send_header(b"Content-type", "text/plain")
        self.end_headers()
        self.wfile.write(b"ok")

    def do_POST(self):
        content_type, _ = cgi.parse_header(self.headers.get('content-type'))

        # refuse to receive non-json content
        if content_type != 'application/json':
            self.send_response(400)
            self.end_headers()
            return

        length = int(self.headers.get('content-length'))
        payload = json.loads(self.rfile.read(length))
        messages = []
        for alert in payload["alerts"]:
            summary = alert['annotations']['summary']
            messages.append(f"{alert['status']}: {summary}")
        irc_send(self.irc_url, messages)

        self.do_GET()


def systemd_socket_response() -> None:
    irc_url = os.environ.get("IRC_URL", None)
    if irc_url is None:
        print("IRC_URL environment variable not set: i.e. IRC_URL=irc+tls://mic92-prometheus@chat.freenode.de#krebs-announce", file=sys.stderr)
        sys.exit(1)

    nfds = os.environ.get("LISTEN_FDS", None)
    if nfds is None:
        print("LISTEN_FDS not set. Run me with systemd(TM) socket activation?",
              file=sys.stderr)
        sys.exit(1)
    fds = range(3, 3 + int(nfds))

    for fd in fds:
        sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(0)

        try:
            while True:
                PrometheusWebHook(irc_url, *sock.accept())
        except BlockingIOError:
            # no more connections
            pass


if __name__ == "__main__":
    systemd_socket_response()
