#!/usr/bin/env python3

import os
import sys
import json

from buildbot.plugins import worker, changes, util, steps, schedulers, reporters
from buildbot.process.properties import Interpolate
from pathlib import Path
from typing import Any


def read_secret_file(secret_name: str) -> str:
    directory = os.environ.get("CREDENTIALS_DIRECTORY")
    if directory is None:
        print(f"directory not set", file=sys.stderr)
        sys.exit(1)
    return Path(directory).joinpath(secret_name).read_text()


def build_config() -> dict[str, Any]:
    c = {}
    c["buildbotNetUsageData"] = None

    #pr_filter = util.ChangeFilter(
    #    filter_fn=lambda change: change.properties["event"] == "pull_request"
    #)

    c["schedulers"] = [
        schedulers.AnyBranchScheduler(
            #name="all", change_filter=pr_filter, builderNames=["runtests"]
            name="all", builderNames=["flake"]
        ),
        schedulers.ForceScheduler(name="force", builderNames=["flake"]),
    ]

    github_api_token = read_secret_file("github-token")
    c["services"] = []
    c["services"].append(
        reporters.GitHubStatusPush(
            token=github_api_token,
            context=Interpolate("buildbot/%(prop:buildername)s"),
        )
    )

    worker_config = json.loads(read_secret_file("github-workers"))

    c["workers"] = [worker.Worker(item["name"], item["pass"]) for item in worker_config]

    factory = util.BuildFactory()
    # check out the source
    factory.addStep(
        steps.GitHub(
            repourl=util.Property("repository"), mode="incremental", submodules=True
        )
    )

    factory.addStep(steps.ShellCommand(command=["nix", "flake", "check"]))

    workernames = [item["name"] for item in worker_config]

    c["builders"] = []
    c["builders"].append(
        util.BuilderConfig(name="flake", workernames=workernames, factory=factory)
    )

    auth_config = util.GitHubAuth(os.environ.get("GITHUB_OAUTH_ID"), read_secret_file("github-oauth-secret"))
    github_admins = os.environ.get("GITHUB_ADMINS", "").split(",")
    authz_config = util.Authz(
        roleMatchers=[util.RolesFromUsername(roles=["admin"], usernames=github_admins)],
        allowRules=[util.AnyEndpointMatcher(role="admin", defaultDeny=False)],
    )
    change_hook_dialects = {}
    change_hook_dialects["github"] = {
        "secret": read_secret_file("github-webhook-secret"),
        "strict": True,
        "token": github_api_token,
        "github_property_whitelist": "*",
    }

    c["www"] = {
        "port": int(os.environ.get("PORT", "1810")),
        "auth": auth_config,
        "authz": authz_config,
        "plugins": dict(waterfall_view={}, console_view={}, grid_view={}),
        "change_hook_dialects": change_hook_dialects,
    }

    c["db"] = {
        # This specifies what database buildbot uses to store its state.  You can
        # leave this at its default for all but the largest installations.
        "db_url": os.environ.get("DB_URL", "sqlite:///state.sqlite")
    }

    c['protocols'] = {'pb': {'port': 9989}}
    c['buildbotURL'] = "https://buildbot.thalheim.io/"

    return c


BuildmasterConfig = build_config()
