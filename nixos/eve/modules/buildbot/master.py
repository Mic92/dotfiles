#!/usr/bin/env python3

import json
import os
import sys
from datetime import timedelta
from pathlib import Path
from typing import Any

from buildbot.plugins import reporters, schedulers, secrets, util, worker
from buildbot.process.project import Project
from buildbot.process.properties import Interpolate

# allow to import modules
sys.path.append(str(Path(__file__).parent))

from buildbot_nix import (  # noqa: E402
    nix_build_config,
    nix_eval_config,
    nix_update_flake_config,
)
from github_projects import GithubProject, load_projects  # noqa: E402
from irc_notify import NotifyFailedBuilds  # noqa: E402


def read_secret_file(secret_name: str) -> str:
    directory = os.environ.get("CREDENTIALS_DIRECTORY")
    if directory is None:
        print("directory not set", file=sys.stderr)
        sys.exit(1)
    return Path(directory).joinpath(secret_name).read_text()


GITHUB_OAUTH_ID = os.environ.get("GITHUB_OAUTH_ID")
GITHUB_OAUTH_SECRET = read_secret_file("github-oauth-secret")
GITHUB_TOKEN_SECRET_NAME = "github-token"
GITHUB_TOKEN = read_secret_file(GITHUB_TOKEN_SECRET_NAME)
GITHUB_WEBHOOK_SECRET = read_secret_file("github-webhook-secret")
# Shape of this file:
# [ { "name": "<worker-name>", "pass": "<worker-password>", "cores": "<cpu-cores>" } ]
BUILDBOT_NIX_WORKERS = read_secret_file("buildbot-nix-workers")
REPO_FOR_FLAKE_UPDATE = os.environ["REPO_FOR_FLAKE_UPDATE"]
BUILDBOT_URL = os.environ["BUILDBOT_URL"]
BUILDBOT_GITHUB_USER = os.environ["BUILDBOT_GITHUB_USER"]
NIX_SUPPORTED_SYSTEMS = os.environ["NIX_SUPPORTED_SYSTEMS"].split(" ")
NIX_EVAL_MAX_MEMORY_SIZE = int(os.environ.get("NIX_EVAL_MAX_MEMORY_SIZE", "4096"))


def config_for_project(
    config: dict[str, Any],
    project: GithubProject,
    credentials: str,
    worker_names: list[str],
) -> Project:
    config["projects"].append(Project(project.name))
    config["schedulers"].extend(
        [
            schedulers.SingleBranchScheduler(
                name=f"default-branch-{project.id}",
                change_filter=util.ChangeFilter(
                    repository=project.url,
                    filter_fn=lambda c: c.branch
                    == c.properties.getProperty("github.repository.default_branch"),
                ),
                builderNames=[f"{project.name}/nix-eval"],
            ),
            # this is compatible with bors or github's merge queue
            schedulers.SingleBranchScheduler(
                name=f"merge-queue-{project.id}",
                change_filter=util.ChangeFilter(
                    repository=project.url,
                    branch_re="(gh-readonly-queue/.*|staging|trying)",
                ),
                builderNames=[f"{project.name}/nix-eval"],
            ),
            # build all pull requests
            schedulers.SingleBranchScheduler(
                name=f"prs-{project.id}",
                change_filter=util.ChangeFilter(
                    repository=project.url, category="pull"
                ),
                builderNames=[f"{project.name}/nix-eval"],
            ),
            # this is triggered from `nix-eval`
            schedulers.Triggerable(
                name=f"{project.id}-nix-build",
                builderNames=[f"{project.name}/nix-build"],
            ),
            # allow to manually trigger a nix-build
            schedulers.ForceScheduler(
                name=f"{project.id}-force", builderNames=[f"{project.name}/nix-eval"]
            ),
            # allow to manually update flakes
            schedulers.ForceScheduler(
                name=f"{project.id}-update-flake",
                builderNames=[f"{project.name}/update-flake"],
                buttonName="Update flakes",
            ),
            # updates flakes once a weeek
            schedulers.NightlyTriggerable(
                name=f"{project.id}-update-flake-weekly",
                builderNames=[f"{project.name}/update-flake"],
                hour=3,
                minute=0,
                dayOfWeek=6,
            ),
        ]
    )
    has_cachix_auth_token = os.path.isfile(
        os.path.join(credentials, "cachix-auth-token")
    )
    has_cachix_signing_key = os.path.isfile(
        os.path.join(credentials, "cachix-signing-key")
    )
    config["builders"].extend(
        [
            # Since all workers run on the same machine, we only assign one of them to do the evaluation.
            # This should prevent exessive memory usage.
            nix_eval_config(
                project,
                [worker_names[0]],
                github_token_secret=GITHUB_TOKEN_SECRET_NAME,
                supported_systems=NIX_SUPPORTED_SYSTEMS,
                automerge_users=[BUILDBOT_GITHUB_USER],
                max_memory_size=NIX_EVAL_MAX_MEMORY_SIZE,
            ),
            nix_build_config(
                project,
                worker_names,
                has_cachix_auth_token,
                has_cachix_signing_key,
            ),
            nix_update_flake_config(
                project,
                worker_names,
                github_token_secret=GITHUB_TOKEN_SECRET_NAME,
                github_bot_user=BUILDBOT_GITHUB_USER,
            ),
        ]
    )


PROJECT_CACHE_FILE = Path("github-project-cache.json")


def build_config() -> dict[str, Any]:
    projects = load_projects(GITHUB_TOKEN, PROJECT_CACHE_FILE)
    projects = [p for p in projects if "build-with-buildbot" in p.topics]
    import pprint

    pprint.pprint(projects)
    c: dict[str, Any] = {}
    c["buildbotNetUsageData"] = None
    # configure a janitor which will delete all logs older than one month, and will run on sundays at noon
    c["configurators"] = [
        util.JanitorConfigurator(logHorizon=timedelta(weeks=4), hour=12, dayOfWeek=6)
    ]
    credentials = os.environ.get("CREDENTIALS_DIRECTORY", ".")
    c["schedulers"] = [
        schedulers.SingleBranchScheduler(
            name="nixpkgs",
            change_filter=util.ChangeFilter(
                repository_re=r"https://github\.com/.*/nixpkgs",
                filter_fn=lambda c: c.branch
                == c.properties.getProperty("github.repository.default_branch"),
            ),
            treeStableTimer=20,
            builderNames=["Mic92/dotfiles/update-flake"],
        ),
    ]
    c["builders"] = []
    c["projects"] = []
    c["workers"] = []

    worker_config = json.loads(BUILDBOT_NIX_WORKERS)
    worker_names = []
    for item in worker_config:
        cores = item.get("cores", 0)
        for i in range(cores):
            worker_name = f"{item['name']}-{i}"
            c["workers"].append(worker.Worker(worker_name, item["pass"]))
            worker_names.append(worker_name)

    for project in projects:
        config_for_project(c, project, credentials, worker_names)

    c["services"] = [
        reporters.GitHubStatusPush(
            token=GITHUB_TOKEN,
            # Since we dynamically create build steps,
            # we use `virtual_builder_name` in the webinterface
            # so that we distinguish what has beeing build
            context=Interpolate("buildbot/%(prop:status_name)s"),
        ),
        # Notify on irc
        NotifyFailedBuilds("irc://buildbot|mic92@irc.r:6667/#xxx"),
    ]

    systemd_secrets = secrets.SecretInAFile(dirname=credentials)
    c["secretsProviders"] = [systemd_secrets]

    github_admins = os.environ.get("GITHUB_ADMINS", "").split(",")

    c["www"] = {
        "avatar_methods": [util.AvatarGitHub()],
        "port": int(os.environ.get("PORT", "1810")),
        "auth": util.GitHubAuth(GITHUB_OAUTH_ID, GITHUB_OAUTH_SECRET),
        "authz": util.Authz(
            roleMatchers=[
                util.RolesFromUsername(roles=["admin"], usernames=github_admins)
            ],
            allowRules=[
                util.AnyEndpointMatcher(role="admin", defaultDeny=False),
                util.AnyControlEndpointMatcher(role="admins"),
            ],
        ),
        "plugins": dict(
            base_react={}, waterfall_view={}, console_view={}, grid_view={}
        ),
        "change_hook_dialects": dict(
            github={
                "secret": GITHUB_WEBHOOK_SECRET,
                "strict": True,
                "token": GITHUB_TOKEN,
                "github_property_whitelist": "*",
            }
        ),
    }

    c["db"] = {"db_url": os.environ.get("DB_URL", "sqlite:///state.sqlite")}

    c["protocols"] = {"pb": {"port": "tcp:9989:interface=\\:\\:"}}
    c["buildbotURL"] = BUILDBOT_URL

    return c


BuildmasterConfig = build_config()
