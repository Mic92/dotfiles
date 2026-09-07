#!/usr/bin/env python3
"""tinc invitation hooks that register retiolum nodes in kartei.

Installed as `invitation-created` and `invitation-accepted` in the tinc
confbase of the enrolment host. Usage on that host:

    tinc -n retiolum invite -e KARTEI_NS=<namespace> <node>
    tinc -n retiolum invite --replace <node>
    tinc -n retiolum invite --replace -e KARTEI_NS=<namespace> <node>  # PR still open

Configuration comes from the environment (set by the NixOS module):
KARTEI_HOSTS_JSON, KARTEI_REPO, KARTEI_BASE, KARTEI_GITHUB_APP_ID,
KARTEI_GITHUB_INSTALLATION_ID, KARTEI_GITHUB_KEY_FILE.
"""

import base64
import hashlib
import ipaddress
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

NET4 = "10.243.0.0/16"
NET6 = "42::/16"
BUNDLE_URL = (
    "https://github.com/krebs/kartei/releases/download/bundle/retiolum-hosts.tar.gz"
)
NS_RE = re.compile(r"^[a-z0-9_][a-z0-9_-]*$")


def die(msg: str) -> None:
    print(f"kartei: {msg}", file=sys.stderr)
    sys.exit(1)


def genipv6(ns: str, host: str) -> str:
    """Port of lib/genipv6.nix for netname retiolum."""

    def h(n: int, s: str) -> str:
        return hashlib.sha256(s.encode()).hexdigest()[:n]

    s = h(20, host)
    groups = [s[i : i + 4] for i in range(0, 20, 4)]
    # lib.stringToGroupsOf puts the last chunk first
    groups = [groups[-1], *groups[:-1]]
    return str(ipaddress.IPv6Address(f"42:0:{h(4, ns)}:{':'.join(groups)}"))


def load_hosts() -> dict[str, dict[str, str | None]]:
    hosts: dict[str, dict[str, str | None]] = json.loads(
        Path(os.environ["KARTEI_HOSTS_JSON"]).read_text()
    )
    return hosts


def resolve(node: str) -> tuple[str, str, str | None, bool]:
    """(namespace, ip6, ip4, is_new) for NODE from kartei or KARTEI_NS."""
    known = load_hosts().get(node)
    replace = os.environ.get("REPLACE")
    if known:
        if not replace:
            die(f"{node} is already in kartei ({known['ns']}), use --replace")
        assert known["ip6"] is not None
        return known["ns"] or "", known["ip6"], known["ip4"], False
    ns = os.environ.get("KARTEI_NS", "")
    if not NS_RE.match(ns):
        die("set the namespace with `-e KARTEI_NS=<name>`")
    return ns, genipv6(ns, node), None, True


def created() -> None:
    node = os.environ["NODE"]
    _ns, ip6, ip4, _new = resolve(node)
    lines = [f"Ifconfig = {ip6}/16", f"Route = {NET6}", f"BundleUrl = {BUNDLE_URL}"]
    if ip4:
        lines += [f"Ifconfig = {ip4}/16", f"Route = {NET4}"]
    # Keys for the invitee go into the first chunk, before the separator.
    inv = Path(os.environ["INVITATION_FILE"])
    text = inv.read_text()
    block = "".join(line + "\n" for line in lines)
    sep = text.find("#---")
    inv.write_text(text + block if sep < 0 else text[:sep] + block + text[sep:])


def b64url(data: bytes) -> str:
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def app_jwt(app_id: str, key_file: str) -> str:
    now = int(time.time())
    head = b64url(json.dumps({"alg": "RS256", "typ": "JWT"}).encode())
    body = b64url(
        json.dumps({"iat": now - 60, "exp": now + 540, "iss": app_id}).encode()
    )
    sig = subprocess.run(
        ["openssl", "dgst", "-sha256", "-sign", key_file],
        input=f"{head}.{body}".encode(),
        stdout=subprocess.PIPE,
        check=True,
    ).stdout
    return f"{head}.{body}.{b64url(sig)}"


class GitHub:
    def __init__(self, repo: str, token: str) -> None:
        self.repo = repo
        self.token = token

    @classmethod
    def for_installation(cls, repo: str) -> "GitHub":
        jwt = app_jwt(
            os.environ["KARTEI_GITHUB_APP_ID"], os.environ["KARTEI_GITHUB_KEY_FILE"]
        )
        inst = os.environ["KARTEI_GITHUB_INSTALLATION_ID"]
        r = cls(repo, jwt).api("POST", f"/app/installations/{inst}/access_tokens")
        return cls(repo, r["token"])

    def api(self, method: str, path: str, data: Any = None) -> Any:
        if not path.startswith("/app/"):
            path = f"/repos/{self.repo}{path}"
        req = urllib.request.Request(
            f"https://api.github.com{path}",
            method=method,
            data=None if data is None else json.dumps(data).encode(),
            headers={
                "Authorization": f"Bearer {self.token}",
                "Accept": "application/vnd.github+json",
                "X-GitHub-Api-Version": "2022-11-28",
            },
        )
        try:
            with urllib.request.urlopen(req) as resp:  # noqa: S310 fixed https URL
                return json.load(resp) if resp.status != 204 else None
        except urllib.error.HTTPError as e:
            if method == "GET" and e.code == 404:
                return None
            die(f"{method} {path}: {e.code} {e.read().decode(errors='replace')}")

    def commit_files(
        self, base: str, branch: str, files: dict[str, str], msg: str
    ) -> None:
        head = self.api("GET", f"/git/ref/heads/{base}")["object"]["sha"]
        tree = self.api(
            "POST",
            "/git/trees",
            {
                "base_tree": head,
                "tree": [
                    {"path": p, "mode": "100644", "type": "blob", "content": c}
                    for p, c in files.items()
                ],
            },
        )["sha"]
        commit = self.api(
            "POST", "/git/commits", {"message": msg, "tree": tree, "parents": [head]}
        )["sha"]
        if self.api("GET", f"/git/ref/heads/{branch}"):
            self.api(
                "PATCH", f"/git/refs/heads/{branch}", {"sha": commit, "force": True}
            )
        else:
            self.api(
                "POST", "/git/refs", {"ref": f"refs/heads/{branch}", "sha": commit}
            )

    def open_pr(self, base: str, branch: str, title: str, body: str) -> str:
        owner = self.repo.split("/")[0]
        existing = self.api("GET", f"/pulls?state=open&head={owner}:{branch}")
        if existing:
            url: str = existing[0]["html_url"]
            return url
        pr = self.api(
            "POST",
            "/pulls",
            {"title": title, "head": branch, "base": base, "body": body},
        )
        return str(pr["html_url"])


def pubkey(host_file: Path) -> str:
    for line in host_file.read_text().splitlines():
        k, _, v = line.partition("=")
        if k.strip().lower() == "ed25519publickey":
            return v.strip()
    die(f"no Ed25519PublicKey in {host_file}")
    raise AssertionError


def prune_overlay(host_file: Path) -> None:
    """Drop overlay entries the deployed hosts/ already carries."""
    overlay = host_file.parent
    deployed = Path(os.environ.get("KARTEI_HOSTS_DIR", "/etc/tinc/retiolum/hosts"))
    for f in overlay.iterdir():
        d = deployed / f.name
        if f != host_file and d.is_file() and pubkey(d) == pubkey(f):
            f.unlink()


def accepted() -> None:
    node = os.environ["NODE"]
    host_file = Path(os.environ["HOST_FILE"])
    prune_overlay(host_file)
    ns, ip6, _ip4, new = resolve(node)
    key = pubkey(host_file)

    d = f"{ns}/hosts/{node}/retiolum"
    files = {f"{d}/ed25519.key": key + "\n"}
    if new:
        files[f"{d}/ip6"] = ip6 + "\n"
        files[f"{d}/aliases"] = f"{node}.r\n"
    verb = "add" if new else "rekey"
    who = os.environ.get("PORTAL_SUB") or "tinc invite"
    body = f"Enrolled via {who} on {os.environ.get('NAME', '?')}."
    if not new:
        body += f"\n\nReplaces key `{os.environ['REPLACE']}`."

    # tincd waits for this script, the joiner waits for tincd. Do the
    # GitHub round trips in the background.
    if os.environ.get("KARTEI_FOREGROUND") is None and os.fork():
        return
    repo = os.environ.get("KARTEI_REPO", "krebs/kartei")
    base = os.environ.get("KARTEI_BASE", "master")
    gh = GitHub.for_installation(repo)
    branch = f"enroll/{node}"
    gh.commit_files(base, branch, files, f"{ns}: {verb} {node}")
    print(gh.open_pr(base, branch, f"{ns}: {verb} {node}", body))


def main() -> None:
    {"invitation-created": created, "invitation-accepted": accepted}[sys.argv[1]]()


if __name__ == "__main__":
    main()
