#!/usr/bin/env python3
"""Wait for a Buildkite build to finish, polling every 30s."""

import json
import os
import re
import sys
import time
import urllib.request
from datetime import UTC, datetime


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: bk-wait <buildkite-build-url>", file=sys.stderr)
        sys.exit(1)

    url = sys.argv[1]
    m = re.search(r"buildkite\.com/([^/]+)/([^/]+)/builds/(\d+)", url)
    if not m:
        print(f"Error: Could not parse Buildkite URL: {url}", file=sys.stderr)
        print(
            "Expected format: https://buildkite.com/ORG/PIPELINE/builds/NUMBER",
            file=sys.stderr,
        )
        sys.exit(1)

    org, pipeline, build = m.group(1), m.group(2), m.group(3)

    token = os.environ.get("BUILDKITE_TOKEN")
    if not token:
        print("Error: BUILDKITE_TOKEN not set", file=sys.stderr)
        sys.exit(1)

    api_url = f"https://api.buildkite.com/v2/organizations/{org}/pipelines/{pipeline}/builds/{build}"
    print(f"Waiting for build #{build} ({org}/{pipeline})...")

    while True:
        if not api_url.startswith("https://"):
            print(f"Error: unexpected URL scheme: {api_url}", file=sys.stderr)
            sys.exit(1)
        req = urllib.request.Request(  # noqa: S310
            api_url, headers={"Authorization": f"Bearer {token}"}
        )
        with urllib.request.urlopen(req) as resp:  # noqa: S310
            state = json.loads(resp.read())["state"]

        timestamp = datetime.now(tz=UTC).astimezone().strftime("%H:%M:%S")
        print(f"{timestamp} state: {state}")

        if state == "passed":
            print("✅ Build passed")
            sys.exit(0)
        elif state in ("failed", "canceled", "not_run", "skipped"):
            print(f"❌ Build finished: {state}")
            sys.exit(1)

        time.sleep(30)


if __name__ == "__main__":
    main()
