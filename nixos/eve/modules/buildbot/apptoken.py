#!/usr/bin/env python3

import argparse
import base64
import http.client
import json
import subprocess
import time
from pathlib import Path
from typing import Any


def base64url(data: bytes) -> str:
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode("utf-8")


def rs256_sign(data: str, private_key: str) -> str:
    signature = subprocess.run(
        ["openssl", "dgst", "-binary", "-sha256", "-sign", private_key],
        input=data.encode("utf-8"),
        stdout=subprocess.PIPE,
        check=True,
        text=True,
    ).stdout
    return base64url(signature)


def build_jwt_payload(app_id: str) -> dict[str, Any]:
    jwt_iat_drift = 60
    jwt_exp_delta = 600
    now = int(time.time())
    iat = now - jwt_iat_drift
    jwt_payload = {"iat": iat, "exp": iat + jwt_exp_delta, "iss": app_id}
    return jwt_payload


def request_access_token(
    app_login: str, app_id: str, app_private_key: str, github_host: str = "github.com"
) -> str:
    uri = (
        f"https://api.{github_host}"
        if github_host == "github.com"
        else f"https://{github_host}/api/v3"
    )
    app_installations_uri = f"{uri}/app/installations"

    jwt_payload = json.dumps(build_jwt_payload(app_id)).encode("utf-8")
    json_headers = json.dumps({"alg": "RS256", "typ": "JWT"}).encode("utf-8")
    encoded_jwt_parts = f"{base64url(json_headers)}.{base64url(jwt_payload)}"
    encoded_mac = rs256_sign(encoded_jwt_parts, app_private_key)
    generated_jwt = f"{encoded_jwt_parts}.{encoded_mac}"

    headers = {
        "Authorization": f"Bearer {generated_jwt}",
        "Accept": "application/vnd.github.v3+json",
    }
    conn = http.client.HTTPSConnection(github_host)
    conn.request("GET", app_installations_uri, headers=headers)
    response = conn.getresponse().read().decode("utf-8")

    access_token_url = None
    for item in json.loads(response):
        if item["account"]["login"] == app_login and item["app_id"] == app_id:
            access_token_url = item["access_tokens_url"]
            break
    if not access_token_url:
        raise ValueError("Access token URL not found")

    conn.request("POST", access_token_url, headers={**headers, "Content-Length": "0"})
    response = conn.getresponse().read().decode("utf-8")
    token = json.loads(response).get("token")

    return token


def main() -> None:
    parser = argparse.ArgumentParser(description="Get Github App Token")
    parser.add_argument(
        "--login",
        type=str,
        help="User or organization name that installed the app",
        required=True,
    )
    parser.add_argument("--app-id", type=str, help="Github App ID", required=True)
    parser.add_argument(
        "--app-private-key-file", type=str, help="Github App Private Key", required=True
    )
    parser.add_argument(
        "--github-host", type=str, help="Github Host", default="github.com"
    )
    args = parser.parse_args()
    app_private_key = Path(args.app_private_key_file).read_text()
    token = request_access_token(
        args.app_login, args.app_id, app_private_key, args.github_host
    )
    print(token)


if __name__ == "__main__":
    main()
