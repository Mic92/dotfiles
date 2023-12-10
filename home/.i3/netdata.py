import http.client
import json
import ssl
from typing import Any
from urllib.parse import urlparse

from i3pystatus import IntervalModule
from i3pystatus.core.util import internet, require


class Netdata(IntervalModule):
    hints = dict(markup=True)
    base_url = "http://127.0.0.1:19999"
    ca_file = ""
    color = "#FFFFFF"
    warning_color = "#FFFF00"
    critical_color = "#FF0000"

    format = "OK: {ok}/WARN: {failed}/CRIT: {critical}"

    settings = (
        "base_url",
        "username",
        "password",
        "service_filter",
        "ca_file",
        "color",
        "warning_color",
        "critical_color",
        "format",
    )

    def request(self, path: str) -> dict[str, Any]:
        url = urlparse(self.base_url)
        context = None
        if self.ca_file != "":
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            context.load_verify_locations(self.ca_file)
        conn = http.client.HTTPSConnection(url.netloc, context=context)

        conn.request("GET", path)
        print(path)
        try:
            resp = conn.getresponse().read()
        except http.client.IncompleteRead as err:
            resp = err.partial
        print(resp)
        return json.loads(resp.decode("utf-8"))

    @require(internet)
    def run(self) -> None:
        resp = self.request("/api/v1/alarms?all")
        critical = 0
        warning = 0
        ok = 0
        for service in resp["alarms"]:
            status = service["status"]
            if status in ("CLEAR", "UNDEFINED"):
                ok += 1
            elif status == "WARNING":
                warning += 1
            else:
                critical += 1
        not_ok = warning + critical
        text = self.format.format(
            ok=ok, warning=warning, critical=critical, not_ok=not_ok
        )
        if critical > 0:
            color = self.critical_color
        elif warning > 0:
            color = self.warning_color
        else:
            color = self.color
        self.output = dict(full_text=text, color=color)
