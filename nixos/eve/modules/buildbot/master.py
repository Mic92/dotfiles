#!/usr/bin/env python3

import json
import os
import sys
from datetime import timedelta
from pathlib import Path
from typing import Any

from buildbot.plugins import reporters, schedulers, secrets, util, worker
from buildbot.process.properties import Interpolate

# allow to import modules
sys.path.append(str(Path(__file__).parent))

from buildbot_nix import (  # noqa: E402
    nix_build_config,
    nix_eval_config,
    nix_update_flake_config,
)
from irc_notify import NotifyFailedBuilds  # noqa: E402


def read_secret_file(secret_name: str) -> str:
    directory = os.environ.get("CREDENTIALS_DIRECTORY")
    if directory is None:
        print("directory not set", file=sys.stderr)
        sys.exit(1)
    return Path(directory).joinpath(secret_name).read_text()


ORG = os.environ["GITHUB_ORG"]
REPO = os.environ["GITHUB_REPO"]
BRANCH = os.environ["GITHUB_BRANCH"]
BUILDBOT_URL = os.environ["BUILDBOT_URL"]
BUILDBOT_GITHUB_USER = os.environ["BUILDBOT_GITHUB_USER"]


def build_config() -> dict[str, Any]:
    c = {}
    c["buildbotNetUsageData"] = None

    # configure a janitor which will delete all logs older than one month, and will run on sundays at noon
    c["configurators"] = [
        util.JanitorConfigurator(logHorizon=timedelta(weeks=4), hour=12, dayOfWeek=6)
    ]

    c["schedulers"] = [
        # build all pushes to default branch
        schedulers.SingleBranchScheduler(
            name="default-branch",
            change_filter=util.ChangeFilter(
                repository=f"https://github.com/{ORG}/{REPO}",
                filter_fn=lambda c: c.branch
                == c.properties.getProperty("github.repository.default_branch"),
            ),
            builderNames=["nix-eval"],
        ),
        # this is compatible with bors or github's merge queue
        schedulers.SingleBranchScheduler(
            name="merge-queue",
            change_filter=util.ChangeFilter(
                branch_re="(gh-readonly-queue/.*|staging|trying)",
            ),
            builderNames=["nix-eval"],
        ),
        # build all pull requests
        schedulers.SingleBranchScheduler(
            name="prs",
            change_filter=util.ChangeFilter(
                repository=f"https://github.com/{ORG}/{REPO}", category="pull"
            ),
            builderNames=["nix-eval"],
        ),
        schedulers.SingleBranchScheduler(
            name="flake-sources",
            change_filter=util.ChangeFilter(
                repository=f"https://github.com/{ORG}/nixpkgs", branch="main"
            ),
            treeStableTimer=20,
            builderNames=["nix-update-flake"],
        ),
        # this is triggered from `nix-eval`
        schedulers.Triggerable(
            name="nix-build",
            builderNames=["nix-build"],
        ),
        # allow to manually trigger a nix-build
        schedulers.ForceScheduler(name="force", builderNames=["nix-eval"]),
        # allow to manually update flakes
        schedulers.ForceScheduler(
            name="update-flake",
            builderNames=["nix-update-flake"],
            buttonName="Update flakes",
        ),
        # updates flakes once a weeek
        schedulers.NightlyTriggerable(
            name="update-flake-weekly",
            builderNames=["nix-update-flake"],
            hour=3,
            minute=0,
            dayOfWeek=6,
        ),
    ]

    github_api_token = read_secret_file("github-token")
    c["services"] = [
        reporters.GitHubStatusPush(
            token=github_api_token,
            # Since we dynamically create build steps,
            # we use `virtual_builder_name` in the webinterface
            # so that we distinguish what has beeing build
            context=Interpolate("buildbot/%(prop:virtual_builder_name)s"),
        ),
        # Notify on irc
        NotifyFailedBuilds("irc://buildbot|mic92@irc.r:6667/#xxx"),
    ]

    # Shape of this file:
    # [ { "name": "<worker-name>", "pass": "<worker-password>", "cores": "<cpu-cores>" } ]
    worker_config = json.loads(read_secret_file("buildbot-nix-workers"))

    credentials = os.environ.get("CREDENTIALS_DIRECTORY", ".")
    has_cachix_auth_token = os.path.isfile(
        os.path.join(credentials, "cachix-auth-token")
    )
    has_cachix_signing_key = os.path.isfile(
        os.path.join(credentials, "cachix-signing-key")
    )

    systemd_secrets = secrets.SecretInAFile(dirname=credentials)
    c["secretsProviders"] = [systemd_secrets]
    c["workers"] = []
    worker_names = []
    for item in worker_config:
        cores = item.get("cores", 0)
        for i in range(cores):
            worker_name = f"{item['name']}-{i}"
            c["workers"].append(worker.Worker(worker_name, item["pass"]))
            worker_names.append(worker_name)
    c["builders"] = [
        # Since all workers run on the same machine, we only assign one of them to do the evaluation.
        # This should prevent exessive memory usage.
        nix_eval_config(
            [worker_names[0]],
            github_token_secret="github-token",
            automerge_users=[BUILDBOT_GITHUB_USER],
        ),
        nix_build_config(worker_names, has_cachix_auth_token, has_cachix_signing_key),
        nix_update_flake_config(
            worker_names,
            f"{ORG}/{REPO}",
            github_token_secret="github-token",
            github_bot_user=BUILDBOT_GITHUB_USER,
            branch=BRANCH,
        ),
    ]

    github_admins = os.environ.get("GITHUB_ADMINS", "").split(",")

    c["www"] = {
        "avatar_methods": [util.AvatarGitHub()],
        "port": int(os.environ.get("PORT", "1810")),
        "auth": util.GitHubAuth(
            os.environ.get("GITHUB_OAUTH_ID"), read_secret_file("github-oauth-secret")
        ),
        "authz": util.Authz(
            roleMatchers=[
                util.RolesFromUsername(roles=["admin"], usernames=github_admins)
            ],
            allowRules=[
                util.AnyEndpointMatcher(role="admin", defaultDeny=False),
                util.AnyControlEndpointMatcher(role="admins"),
            ],
        ),
        "plugins": dict(waterfall_view={}, console_view={}, grid_view={}),
        "change_hook_dialects": dict(
            github={
                "secret": read_secret_file("github-webhook-secret"),
                "strict": True,
                "token": github_api_token,
                "github_property_whitelist": "*",
            }
        ),
    }

    c["db"] = {"db_url": os.environ.get("DB_URL", "sqlite:///state.sqlite")}

    c["protocols"] = {"pb": {"port": "tcp:9989:interface=\\:\\:"}}
    c["buildbotURL"] = BUILDBOT_URL

    return c


BuildmasterConfig = build_config()
