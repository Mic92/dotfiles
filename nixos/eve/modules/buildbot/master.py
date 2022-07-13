#!/usr/bin/env python3

import os
import sys
import json

from buildbot.plugins import worker, util, schedulers, reporters, secrets
from buildbot.process.properties import Interpolate
from pathlib import Path
from typing import Any

# allow to import modules
sys.path.append(str(Path(__file__).parent))

from irc_notify import NotifyFailedBuilds
from buildbot_nix import nix_eval_config, nix_build_config


def read_secret_file(secret_name: str) -> str:
    directory = os.environ.get("CREDENTIALS_DIRECTORY")
    if directory is None:
        print(f"directory not set", file=sys.stderr)
        sys.exit(1)
    return Path(directory).joinpath(secret_name).read_text()


def build_config() -> dict[str, Any]:
    c = {}
    c["buildbotNetUsageData"] = None

    c["schedulers"] = [
        schedulers.AnyBranchScheduler(
            name="all",
            builderNames=["nix-eval"],
        ),
        schedulers.Triggerable(
            name="nix-build",
            builderNames=["nix-build"],
        ),
        schedulers.ForceScheduler(name="force", builderNames=["nix-eval"]),
    ]

    github_api_token = read_secret_file("github-token")
    c["services"] = [
        reporters.GitHubStatusPush(
            token=github_api_token,
            context=Interpolate("buildbot/%(prop:virtual_builder_name)s"),
        ),
        NotifyFailedBuilds("irc://buildbot|mic92@irc.r:6667/#xxx"),
    ]

    worker_config = json.loads(read_secret_file("github-workers"))

    credentials = os.environ.get("CREDENTIALS_DIRECTORY", ".")
    enable_cachix = os.path.isfile(os.path.join(credentials, "cachix-token"))

    systemd_secrets = secrets.SecretInAFile(dirname=credentials)
    c["secretsProviders"] = [systemd_secrets]
    c["workers"] = [worker.Worker(item["name"], item["pass"]) for item in worker_config]
    worker_names = [item["name"] for item in worker_config]
    c["builders"] = [
        nix_eval_config(worker_names),
        nix_build_config(worker_names, enable_cachix),
    ]

    github_admins = os.environ.get("GITHUB_ADMINS", "").split(",")

    c["www"] = {
        "port": int(os.environ.get("PORT", "1810")),
        "auth": util.GitHubAuth(
            os.environ.get("GITHUB_OAUTH_ID"), read_secret_file("github-oauth-secret")
        ),
        "authz": util.Authz(
            roleMatchers=[
                util.RolesFromUsername(roles=["admin"], usernames=github_admins)
            ],
            allowRules=[util.AnyEndpointMatcher(role="admin", defaultDeny=False)],
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

    c["protocols"] = {"pb": {"port": 9989}}
    c["buildbotURL"] = "https://buildbot.thalheim.io/"

    return c


BuildmasterConfig = build_config()
