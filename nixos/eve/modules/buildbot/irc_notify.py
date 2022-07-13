from typing import Optional, Generator, Any
import socket
import ssl
import threading
import re
from urllib.parse import urlparse
import base64

from buildbot.reporters.base import ReporterBase
from buildbot.reporters.generators.build import BuildStatusGenerator
from buildbot.reporters.message import MessageFormatter
from twisted.internet import defer

DEBUG = False


def _irc_send(
    server: str,
    nick: str,
    channel: str,
    sasl_password: Optional[str] = None,
    server_password: Optional[str] = None,
    tls: bool = True,
    port: int = 6697,
    messages: list[str] = [],
) -> None:
    if not messages:
        return

    # don't give a shit about legacy ip
    sock = socket.socket(family=socket.AF_INET6)
    if tls:
        sock = ssl.wrap_socket(
            sock, cert_reqs=ssl.CERT_NONE, ssl_version=ssl.PROTOCOL_TLSv1_2
        )

    def _send(command: str) -> int:
        if DEBUG:
            print(command)
        return sock.send((f"{command}\r\n").encode())

    def _pong(ping: str):
        if ping.startswith("PING"):
            sock.send(ping.replace("PING", "PONG").encode("ascii"))

    recv_file = sock.makefile(mode="r")

    print(f"connect {server}:{port}")
    sock.connect((server, port))
    if server_password:
        _send(f"PASS {server_password}")
    _send(f"USER {nick} 0 * :{nick}")
    _send(f"NICK {nick}")
    for line in recv_file.readline():
        if re.match(r"^:[^ ]* (MODE|221|376|422) ", line):
            break
        else:
            _pong(line)

    if sasl_password:
        _send("CAP REQ :sasl")
        _send("AUTHENTICATE PLAIN")
        auth = base64.encodebytes(f"{nick}\0{nick}\0{sasl_password}".encode("ascii"))
        _send(f"AUTHENTICATE {auth.decode('ascii')}")
        _send("CAP END")
    _send(f"JOIN :{channel}")

    for m in messages:
        _send(f"PRIVMSG {channel} :{m}")

    _send("INFO")
    for line in recv_file:
        if DEBUG:
            print(line, end="")
        # Assume INFO reply means we are done
        if "End of /INFO" in line:
            break
        else:
            _pong(line)

    sock.send(b"QUIT")
    print("disconnect")
    sock.close()


def irc_send(
    url: str, notifications: list[str], password: Optional[str] = None
) -> None:
    parsed = urlparse(f"{url}")
    username = parsed.username or "prometheus"
    server = parsed.hostname or "chat.freenode.net"
    if parsed.fragment != "":
        channel = f"#{parsed.fragment}"
    else:
        channel = "#krebs-announce"
    port = parsed.port or 6697
    if not password:
        password = parsed.password
    if len(notifications) == 0:
        return
    # put this in a thread to not block buildbot
    t = threading.Thread(
        target=_irc_send,
        kwargs=dict(
            server=server,
            nick=username,
            sasl_password=password,
            channel=channel,
            port=port,
            messages=notifications,
            tls=parsed.scheme == "irc+tls",
        ),
    )
    t.start()


subject_template = """\
{{ '☠' if result_names[results] == 'failure' else '☺' if result_names[results] == 'success' else '☝' }} \
{{ build['properties'].get('project', ['whole buildset'])[0] if is_buildset else buildername }} \
- \
{{ build['state_string'] }} \
{{ '(%s)' % (build['properties']['branch'][0] if (build['properties']['branch'] and build['properties']['branch'][0]) else build['properties'].get('got_revision', ['(unknown revision)'])[0]) }} \
({{ build_url }})
"""  # # noqa pylint: disable=line-too-long


class NotifyFailedBuilds(ReporterBase):
    def _generators(self) -> list[BuildStatusGenerator]:
        formatter = MessageFormatter(template_type="plain", subject=subject_template)
        return [BuildStatusGenerator(message_formatter=formatter)]

    def checkConfig(self, url: str):
        super().checkConfig(generators=self._generators())

    @defer.inlineCallbacks
    def reconfigService(self, url: str) -> Generator[Any, object, Any]:
        self.url = url
        yield super().reconfigService(generators=self._generators())

    def sendMessage(self, reports: list):
        msgs = []
        for r in reports:
            if r["builds"][0]["state_string"] != "build successful":
                msgs.append(r["subject"])
        irc_send(self.url, notifications=msgs)
