import http.client
import json
import ssl
from base64 import b64encode
from typing import Any, Dict, Optional, Union
from urllib.parse import urlparse

from i3pystatus import IntervalModule
from i3pystatus.core.util import internet, require


class PasswordCmd:
    def get(self) -> str:
        raise Exception("not implemented")


class Icinga(IntervalModule):
    hints = dict(markup=True)
    ca_file = ""
    password: Union[str, PasswordCmd]
    service_filter = ""
    color = "#FFFFFF"
    warning_color = "#FFFF00"
    critical_color = "#FF0000"

    format = "OK: {ok}/WARN: {failed}/CRIT: {critical}"

    required = ("base_url", "username", "password")
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

    def request(self, path: str, body: Optional[str]) -> Dict[str, Any]:
        url = urlparse(self.base_url)
        context = None
        if self.ca_file != "":
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            context.load_verify_locations(self.ca_file)
        conn = http.client.HTTPSConnection(url.netloc, context=context)

        if isinstance(self.password, str):
            password = self.password
        else:
            password = self.password.get()
        user_and_pass = b64encode(f"{self.username}:{password}".encode("utf-8")).decode(
            "ascii"
        )
        headers = {
            "Authorization": "Basic %s" % user_and_pass,
            "Accept": "application/json",
            "X-HTTP-Method-Override": "GET",
        }
        conn.request("POST", path, headers=headers, body=body)
        resp = conn.getresponse().read()
        return json.loads(resp.decode("utf-8"))

    @require(internet)
    def run(self) -> None:
        body = None
        if self.service_filter != "":
            body = json.dumps(dict(filter=self.service_filter))
        resp = self.request("/v1/objects/services", body)
        critical = 0
        warning = 0
        ok = 0
        for service in resp["results"]:
            status = service["attrs"]["last_check_result"]["exit_status"]
            if status == 0:
                ok += 1
            elif status == 1:
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
