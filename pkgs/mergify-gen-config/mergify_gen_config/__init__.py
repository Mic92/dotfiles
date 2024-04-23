#!/usr/bin/env python3

import argparse
import json
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request
from pathlib import Path
from shutil import which
from typing import Any, NoReturn

import ruamel.yaml


class GithubClient:
    def __init__(self, api_token: str | None) -> None:
        self.api_token = api_token

    def _request(
        self, path: str, method: str, data: dict[str, Any] | None = None
    ) -> Any:
        url = urllib.parse.urljoin("https://api.github.com/", path)
        print(url)
        headers = {"Content-Type": "application/json"}
        if self.api_token:
            headers["Authorization"] = f"token {self.api_token}"

        body = None
        if data:
            body = json.dumps(data).encode("ascii")

        if not url.startswith("https://"):
            msg = f"unexpected url {url}"
            raise ValueError(msg)
        req = urllib.request.Request(  # noqa: S310
            url, headers=headers, method=method, data=body
        )
        try:
            resp = urllib.request.urlopen(req)  # noqa: S310
        except urllib.error.HTTPError as e:
            if e.status != 307:
                raise  # not a status code that can be handled here
            # retry with the new location
            url = e.headers["Location"]
            if not url.startswith("https://"):
                msg = f"unexpected redirect to {url}"
                raise ValueError(msg) from e
            req = urllib.request.Request(  # noqa: S310
                e.headers["Location"], headers=headers, method=method, data=body
            )
            resp = urllib.request.urlopen(req)  # noqa: S310
        return json.loads(resp.read())

    def get(self, path: str) -> Any:
        return self._request(path, "GET")

    def post(self, path: str, data: dict[str, str]) -> Any:
        return self._request(path, "POST", data)

    def put(self, path: str) -> Any:
        return self._request(path, "PUT")

    def fetch_check_suites(self, owner: str, repo: str, ref: str) -> dict[str, Any]:
        return self.get(f"repos/{owner}/{repo}/commits/{ref}/check-suites")

    def fetch_repo(self, owner: str, repo: str) -> dict[str, Any]:
        return self.get(f"repos/{owner}/{repo}")

    def fetch_check_runs(
        self,
        owner: str,
        repo: str,
        check_id: str,
    ) -> dict[str, Any]:
        return self.get(f"repos/{owner}/{repo}/check-suites/{check_id}/check-runs")

    def fetch_labels(self, owner: str, repo: str) -> list[dict[str, Any]]:
        return self.get(f"repos/{owner}/{repo}/labels")

    def create_label(
        self, owner: str, repo: str, name: str, color: str, description: str
    ) -> None:
        self.post(
            f"repos/{owner}/{repo}/labels",
            {
                "name": name,
                "color": color,
                "description": description,
            },
        )


def hub_config_path() -> Path:
    raw_hub_path = os.environ.get("HUB_CONFIG", None)
    if raw_hub_path:
        return Path(raw_hub_path)
    raw_config_home = os.environ.get("XDG_CONFIG_HOME", None)
    if raw_config_home is None:
        config_home = Path.home().joinpath(".config")
    else:
        config_home = Path(raw_config_home)
    return config_home.joinpath("hub")


def read_github_token() -> str | None:
    # for backwards compatibility we also accept GITHUB_OAUTH_TOKEN.
    token = os.environ.get("GITHUB_OAUTH_TOKEN", os.environ.get("GITHUB_TOKEN"))
    if token:
        return token
    try:
        with hub_config_path().open() as f:
            for line in f:
                # Allow substring match as hub uses yaml. Example string we match:
                #  - oauth_token: ghp_abcdefghijklmnopqrstuvwxyzABCDEF1234\n
                token_match = re.search(
                    r"\s*oauth_token:\s+((?:gh[po]_)?[A-Za-z0-9]+)", line
                )
                if token_match:
                    return token_match.group(1)
    except OSError:
        pass
    if which("gh"):
        r = subprocess.run(
            ["gh", "auth", "token"], stdout=subprocess.PIPE, text=True, check=False
        )
        if r.returncode == 0:
            return r.stdout.strip()
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


def get_github_info() -> tuple[str, str] | None:
    # Get the remote origin URL
    res = subprocess.run(
        ["git", "config", "--get", "remote.origin.url"],
        stdout=subprocess.PIPE,
        text=True,
        check=False,
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


yaml = ruamel.yaml.YAML()  # also preserves comments
yaml.indent(mapping=2, sequence=4, offset=2)


def update_mergify_config(mergify_config: Path, runs: list[str]) -> dict[str, Any]:
    with mergify_config.open() as stream:
        config = yaml.load(stream)
        for rule in config["queue_rules"]:
            new_rules = ["check-success=" + run for run in runs]
            if rule["name"] == "default":
                new_rules.extend(
                    check
                    for check in rule["merge_conditions"]
                    if not check.startswith("check-success=")
                )
                rule["merge_conditions"] = new_rules

        config["defaults"].setdefault("actions", {})
        config["defaults"]["actions"].setdefault("queue", {})
        config["defaults"]["actions"]["queue"]["merge_method"] = "rebase"
        if "method" in config["defaults"]["actions"]["queue"]:
            del config["defaults"]["actions"]["queue"]["method"]
        if (
            "allow_merging_configuration_change"
            in config["defaults"]["actions"]["queue"]
        ):
            del config["defaults"]["actions"]["queue"][
                "allow_merging_configuration_change"
            ]

        return config


def new_mergify_config(
    client: GithubClient, args: argparse.Namespace, check_runs: list[str]
) -> dict[str, Any]:
    # queue_rules:
    #  - name: default
    #    merge_conditions:
    #      - check-success=buildbot/nix-eval
    # defaults:
    #    actions:
    #      queue:
    #        merge_method: rebase
    # pull_request_rules:
    #  - name: merge using the merge queue
    #    conditions:
    #      - base=main
    #      - label~=merge-queue|dependencies
    #    actions:
    #      queue:
    default_branch = client.fetch_repo(args.owner, args.repo)["default_branch"]
    return {
        "queue_rules": [
            {
                "name": "default",
                "merge_conditions": [
                    f"check-success={check_run}" for check_run in sorted(check_runs)
                ],
            }
        ],
        "defaults": {
            "actions": {
                "queue": {
                    "merge_method": "rebase",
                }
            }
        },
        "pull_request_rules": [
            {
                "name": "merge using the merge queue",
                "conditions": [
                    f"base={default_branch}",
                    # merge-queue or dependabot
                    "label~=merge-queue|dependencies",
                ],
                "actions": {"queue": {}},
            }
        ],
    }


def ensure_merge_label(client: GithubClient, args: argparse.Namespace) -> None:
    labels = client.fetch_labels(args.owner, args.repo)
    if any(label["name"] == "merge-queue" for label in labels):
        return
    client.create_label(
        args.owner, args.repo, "merge-queue", "05B11B", "merge after ci succeeds"
    )


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
    check_run_set = set()
    for suite in suites["check_suites"]:
        runs = client.fetch_check_runs(args.owner, args.repo, suite["id"])
        for r in runs["check_runs"]:
            if r["app"]["slug"] == "mergify":
                continue
            # flake update lockfile is not a check
            if r["name"] == "lockfile":
                continue
            check_run_set.add(r["name"])
    check_runs = sorted(check_run_set)
    mergify_config = Path(".mergify.yml")
    if mergify_config.exists():
        rules = update_mergify_config(mergify_config, check_runs)
    else:
        rules = new_mergify_config(client, args, check_runs)
    with mergify_config.open("w") as f:
        yaml.dump(rules, f)
    ensure_merge_label(client, args)


if __name__ == "__main__":
    main()
# vim: set filetype=python
