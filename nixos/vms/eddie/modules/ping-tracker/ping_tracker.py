import http.client
import json
import subprocess
import sys
from dataclasses import dataclass
from typing import List


@dataclass
class Device:
    name: str
    host: str
    mac: str
    location: str
    reachable: bool = False


def update_status(url: str, token: str, device: Device):
    conn = http.client.HTTPSConnection(url)

    headers = {
        "Authorization": f"Bearer {token}",
        "content-type": "application/json",
    }
    location = device.location if device.reachable else "not_home"
    body = dict(mac=device.mac, host_name=device.name, location_name=location)
    conn.request("POST", "/api/services/device_tracker/see",
                 headers=headers, body=json.dumps(body))
    resp = conn.getresponse().read()
    return json.loads(resp.decode("utf-8"))


def ping_hosts(devices: List[Device]):
    hosts = [device.host for device in devices]
    devices_by_name = dict([(device.host, device) for device in devices])
    proc = subprocess.run(["fping"] + hosts, stdout=subprocess.PIPE)
    output = proc.stdout.decode("utf-8").rstrip()
    for line in output.split("\n"):
        columns = line.split(" ")
        host = columns[0]
        reachable = columns[2] == "alive"
        devices_by_name[host].reachable = reachable


def main() -> None:
    if len(sys.argv) < 2:
        print(f"USAGE: {sys.argv[0]} config.json")
    with open(sys.argv[1]) as f:
        config = json.load(f)

    url: str = config["url"]
    token: str = config["api_token"]
    devices: List[Device] = []

    for raw_dev in config["devices"]:
        devices.append(Device(**raw_dev))

    ping_hosts(devices)
    for device in devices:
        update_status(url, token, device)


if __name__ == '__main__':
    main()
