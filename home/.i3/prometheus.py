import http.client
import json
from typing import Any
from urllib.parse import urlparse

from i3pystatus import IntervalModule
from i3pystatus.core.util import internet, require


class Prometheus(IntervalModule):
    hints = dict(markup=True)
    color = "#FFFFFF"
    warning_color = "#FFFF00"
    critical_color = "#FF0000"

    format = "PEN/FIR: {pending}/{firing}"

    required = ("base_url",)
    settings = (
        "base_url",
        "color",
        "warning_color",
        "critical_color",
        "format",
    )

    def request(self, path: str) -> dict[str, Any]:
        url = urlparse(self.base_url)
        if url.scheme == "http":
            conn = http.client.HTTPConnection(url.netloc)
        else:
            conn = http.client.HTTPSConnection(url.netloc)
        conn.request("GET", path)
        resp = conn.getresponse().read()
        return json.loads(resp.decode("utf-8"))

    @require(internet)
    def run(self) -> None:
        try:
            resp = self.request("/api/v1/alerts")
        except Exception:
            self.output = dict(full_text="N/A", color=self.critical_color)
            return

        pending = 0
        firing = 0
        for alert in resp["data"]["alerts"]:
            if alert["state"] == "pending":
                pending += 1
            else:
                firing += 1
        not_ok = pending + firing
        text = self.format.format(
            pending=pending, firing=firing, not_ok=not_ok
        )
        if firing > 0:
            color = self.critical_color
        elif pending > 0:
            color = self.warning_color
        else:
            color = self.color
        self.output = dict(full_text=text, color=color)
