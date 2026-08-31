#!/usr/bin/env python3
"""Reconcile lldap service users/groups with a declarative JSON spec.

Managed user and group names are recorded in a state file so entries dropped
from the spec are deleted on the next run. Anything created by hand in the
web UI is left alone.

Passwords are set with lldap_set_password (OPAQUE needs client-side work),
but only when a test login fails, to avoid rewriting the record on every
activation.
"""

import argparse
import http.client
import json
import os
import subprocess
import sys
from http import HTTPStatus
from pathlib import Path
from typing import Any


class LldapError(Exception):
    pass


class Lldap:
    def __init__(self, host: str, port: int, set_password_bin: str) -> None:
        self.host = host
        self.port = port
        self.set_password_bin = set_password_bin
        self.token = ""

    def _post(self, path: str, body: dict[str, Any]) -> tuple[int, Any]:
        conn = http.client.HTTPConnection(self.host, self.port)
        try:
            conn.request(
                "POST",
                path,
                json.dumps(body),
                {
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {self.token}",
                },
            )
            resp = conn.getresponse()
            data = resp.read()
        finally:
            conn.close()
        if resp.status not in (HTTPStatus.OK, HTTPStatus.UNAUTHORIZED):
            msg = f"POST {path}: {resp.status} {data.decode(errors='replace')}"
            raise LldapError(msg)
        return resp.status, json.loads(data) if data else None

    def try_login(self, username: str, password: str) -> str | None:
        status, body = self._post(
            "/auth/simple/login", {"username": username, "password": password}
        )
        return None if status == HTTPStatus.UNAUTHORIZED else body["token"]

    def gql(self, query: str, **variables: Any) -> dict[str, Any]:
        status, body = self._post(
            "/api/graphql", {"query": query, "variables": variables}
        )
        if status != HTTPStatus.OK or body.get("errors"):
            msg = f"graphql {status}: {body}"
            raise LldapError(msg)
        data: dict[str, Any] = body["data"]
        return data

    def set_password(self, user: str, password: str) -> None:
        subprocess.run(
            [
                self.set_password_bin,
                f"--base-url=http://{self.host}:{self.port}",
                f"--token={self.token}",
                f"--username={user}",
                "--bypass-password-policy",
            ],
            env={**os.environ, "LLDAP_USER_PASSWORD": password},
            check=True,
        )


def ensure_user(
    api: Lldap,
    uid: str,
    spec: dict[str, Any],
    have_groups: set[str] | None,
    group_ids: dict[str, int],
) -> None:
    attrs = {"id": uid, "email": spec["email"], "displayName": spec["displayName"]}
    if have_groups is None:
        print(f"creating user {uid}")
        api.gql("mutation($u:CreateUserInput!){createUser(user:$u){id}}", u=attrs)
        have_groups = set()
    else:
        api.gql("mutation($u:UpdateUserInput!){updateUser(user:$u){ok}}", u=attrs)

    want_groups = set(spec["groups"])
    for g in want_groups - have_groups:
        print(f"adding {uid} to {g}")
        api.gql(
            "mutation($u:String!,$g:Int!){addUserToGroup(userId:$u,groupId:$g){ok}}",
            u=uid,
            g=group_ids[g],
        )
    for g in have_groups - want_groups:
        print(f"removing {uid} from {g}")
        api.gql(
            "mutation($u:String!,$g:Int!){removeUserFromGroup(userId:$u,groupId:$g){ok}}",
            u=uid,
            g=group_ids[g],
        )

    if spec["passwordFile"] is not None:
        password = Path(spec["passwordFile"]).read_text().rstrip("\n")
        if api.try_login(uid, password) is None:
            print(f"setting password for {uid}")
            api.set_password(uid, password)


def reconcile(api: Lldap, spec: dict[str, Any], state_path: Path) -> None:
    state = json.loads(state_path.read_text()) if state_path.exists() else {}

    data = api.gql("{ groups { id displayName } users { id groups { displayName } } }")
    group_ids = {g["displayName"]: g["id"] for g in data["groups"]}
    users = {u["id"]: {g["displayName"] for g in u["groups"]} for u in data["users"]}

    for name in spec["groups"]:
        if name not in group_ids:
            print(f"creating group {name}")
            res = api.gql("mutation($n:String!){createGroup(name:$n){id}}", n=name)
            group_ids[name] = res["createGroup"]["id"]

    for uid, u in spec["users"].items():
        ensure_user(api, uid, u, users.get(uid), group_ids)

    for uid in set(state.get("users", [])) - set(spec["users"]):
        if uid in users:
            print(f"deleting formerly managed user {uid}")
            api.gql("mutation($u:String!){deleteUser(userId:$u){ok}}", u=uid)
    for name in set(state.get("groups", [])) - set(spec["groups"]):
        if name in group_ids:
            print(f"deleting formerly managed group {name}")
            api.gql("mutation($g:Int!){deleteGroup(groupId:$g){ok}}", g=group_ids[name])

    state_path.write_text(
        json.dumps({"users": sorted(spec["users"]), "groups": spec["groups"]})
    )


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--host", default="127.0.0.1")
    p.add_argument("--port", type=int, default=17170)
    p.add_argument("--admin-user", default="admin")
    p.add_argument("--admin-password-file", required=True, type=Path)
    p.add_argument("--spec", required=True, type=Path)
    p.add_argument("--state", required=True, type=Path)
    p.add_argument("--set-password-bin", default="lldap_set_password")
    args = p.parse_args()

    api = Lldap(args.host, args.port, args.set_password_bin)
    token = api.try_login(
        args.admin_user, args.admin_password_file.read_text().rstrip("\n")
    )
    if token is None:
        sys.exit(f"error: login as {args.admin_user} failed")
    api.token = token
    try:
        reconcile(api, json.loads(args.spec.read_text()), args.state)
    except LldapError as e:
        sys.exit(f"error: {e}")


if __name__ == "__main__":
    main()
