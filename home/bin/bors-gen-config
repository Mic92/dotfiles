#!/usr/bin/env python3

import argparse
import sys
import os
import re
import urllib.request
import urllib.parse
import subprocess
from string import Template
from textwrap import dedent
from typing import Optional, Any, Tuple, NoReturn
import json
from pathlib import Path


class GithubClient:
    def __init__(self, api_token: Optional[str]) -> None:
        self.api_token = api_token

    def _request(
        self, path: str, method: str, data: Optional[dict[str, Any]] = None
    ) -> Any:
        url = urllib.parse.urljoin("https://api.github.com/", path)
        print(url)
        headers = {"Content-Type": "application/json"}
        if self.api_token:
            headers["Authorization"] = f"token {self.api_token}"

        body = None
        if data:
            body = json.dumps(data).encode("ascii")

        req = urllib.request.Request(url, headers=headers, method=method, data=body)
        resp = urllib.request.urlopen(req)
        return json.loads(resp.read())

    def get(self, path: str) -> Any:
        return self._request(path, "GET")

    def post(self, path: str, data: dict[str, str]) -> Any:
        return self._request(path, "POST", data)

    def put(self, path: str) -> Any:
        return self._request(path, "PUT")

    def fetch_check_suites(self, owner: str, repo: str, ref: str) -> dict[str, Any]:
        return self.get(f"repos/{owner}/{repo}/commits/{ref}/check-suites")

    def fetch_check_runs(
        self,
        owner: str,
        repo: str,
        ref: str,
        id: str,
    ) -> dict[str, Any]:
        return self.get(f"repos/{owner}/{repo}/check-suites/{id}/check-runs")


def hub_config_path() -> Path:
    raw_hub_path = os.environ.get("HUB_CONFIG", None)
    if raw_hub_path:
        return Path(raw_hub_path)
    else:
        raw_config_home = os.environ.get("XDG_CONFIG_HOME", None)
        if raw_config_home is None:
            config_home = Path.home().joinpath(".config")
        else:
            config_home = Path(raw_config_home)
        return config_home.joinpath("hub")


def read_github_token() -> Optional[str]:
    # for backwards compatibility we also accept GITHUB_OAUTH_TOKEN.
    token = os.environ.get("GITHUB_OAUTH_TOKEN", os.environ.get("GITHUB_TOKEN"))
    if token:
        return token
    paths = [hub_config_path(), Path.home().joinpath(".config", "gh", "hosts.yml")]
    for path in paths:
        try:
            with open(path) as f:
                for line in f:
                    # Allow substring match as hub uses yaml. Example string we match:
                    # " - oauth_token: ghp_abcdefghijklmnopqrstuvwxyzABCDEF1234\n"
                    token_match = re.search(
                        r"\s*oauth_token:\s+((?:gh[po]_)?[A-Za-z0-9]+)", line
                    )
                    if token_match:
                        return token_match.group(1)
        except OSError:
            pass
    return None


def parse_args(args: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-r",
        "--repo",
        help="The repository to fetch for (defaults to the detected repository in the current directory)",
    )
    parser.add_argument(
        "-o",
        "--owner",
        help="The owner to fetch for (defaults to the detected repository in the current directory)",
    )
    parser.add_argument(
        "-b", "--branch", default="HEAD", help="The branch to fetch for"
    )

    return parser.parse_args(args)


def get_github_info() -> Optional[Tuple[str, str]]:
    # Get the remote origin URL
    res = subprocess.run(
        ["git", "config", "--get", "remote.origin.url"],
        stdout=subprocess.PIPE,
        text=True,
    )
    if res.returncode != 0:
        return None
    # i.e. https://github.com/Mic92/dotfiles.git or 'git@github.com:Mic92/ssh-to-age.git\n'
    out = res.stdout.strip()
    match = re.match("https://github.com/([^/]+)/([^/]+)", out)
    if not match:
        match = re.match("git@github.com:([^/]+)/([^/]+)", out)
        if not match:
            return None

    username = match.groups()[0]
    repo = re.sub(".git$", "", match.groups()[1])

    return (username, repo)


def die(msg: str) -> NoReturn:
    print(msg, file=sys.stderr)
    sys.exit(1)


def main(_args: list[str] = sys.argv[1:]) -> None:
    args = parse_args(_args)
    if args.repo is None or args.owner is None:
        info = get_github_info()

        if args.repo is None:
            if info is None:
                die("Please provide a repo with `--repo`")
            args.repo = args.repo or info[1]

        if args.owner is None:
            if info is None:
                die("Please provide an owner with `--owner`")
            args.owner = args.owner or info[0]

    client = GithubClient(read_github_token())
    suites = client.fetch_check_suites(args.owner, args.repo, args.branch)
    check_runs = set()
    for suite in suites["check_suites"]:
        runs = client.fetch_check_runs(args.owner, args.repo, args.branch, suite["id"])
        for r in runs["check_runs"]:
            if r["app"]["slug"] == "mergify":
                continue
            check_runs.add(r["name"])
    status = json.dumps(list(sorted(check_runs)), indent=2)
    text = f"""
      cut_body_after = "" # don't include text from the PR body in the merge commit message
      status = $status
    """
    rendered = Template(dedent(text)).substitute(dict(status=status)).lstrip()
    print(rendered)
    p = Path("bors.toml")
    p.write_text(rendered)



if __name__ == "__main__":
    main()
