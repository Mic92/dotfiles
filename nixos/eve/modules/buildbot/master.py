#!/usr/bin/env python3

import os
import sys
import json

from buildbot.plugins import worker, changes, util, steps, schedulers, reporters
from buildbot.process import buildstep, logobserver
from buildbot.process.properties import Interpolate
from typing import Generator
from pathlib import Path
from typing import Any, Dict, List
from twisted.internet import defer


def read_secret_file(secret_name: str) -> str:
    directory = os.environ.get("CREDENTIALS_DIRECTORY")
    if directory is None:
        print(f"directory not set", file=sys.stderr)
        sys.exit(1)
    return Path(directory).joinpath(secret_name).read_text()


class GenerateStagesCommand(buildstep.ShellMixin, steps.BuildStep):
    def __init__(self, **kwargs):
        kwargs = self.setupShellMixin(kwargs)
        super().__init__(**kwargs)
        self.observer = logobserver.BufferLogObserver()
        self.addLogObserver("stdio", self.observer)

    @defer.inlineCallbacks
    def run(self) -> Generator[Any, object, Any]:
        # run nix-instanstiate to generate the dict of stages
        cmd = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)

        # if the command passes extract the list of stages
        result = cmd.results()
        if result == util.SUCCESS:
            # create a ShellCommand for each stage and add them to the build
            jobs = []
            for line in self.observer.getStdout().split("\n"):
                if line != "":
                    jobs.append(json.loads(line))
            build_steps = []
            for job in jobs:
                build_steps.append(
                    steps.ShellCommand(
                        name=job["attr"],
                        command=f"nix build --out-link 'result-{job['attr']}' -L '{job['drvPath']}'",
                    )
                )
            self.build.addStepsAfterCurrentStep(build_steps)

        return result


def nix_builder_config(worker_config: List[Dict[str, str]]) -> util.BuilderConfig:
    factory = util.BuildFactory()
    # check out the source
    factory.addStep(
        steps.GitHub(
            repourl=util.Property("repository"), method="clean", submodules=True
        )
    )

    factory.addStep(
        GenerateStagesCommand(
            env={},
            name="Generate build stages",
            command=[
                "nix",
                "run",
                "github:nix-community/nix-eval-jobs",
                "--",
                "--workers", "8",
                "--gc-roots-dir",
                "/var/lib/buildbot-worker/gcroot",
                "--flake",
                ".#hydraJobs"
            ],
            haltOnFailure=True,
        )
    )

    worker_names = [item["name"] for item in worker_config]
    return util.BuilderConfig(name="flake", workernames=worker_names, factory=factory)


def build_config() -> dict[str, Any]:
    c = {}
    c["buildbotNetUsageData"] = None

    # pr_filter = util.ChangeFilter(
    #    filter_fn=lambda change: change.properties["event"] == "pull_request"
    # )

    c["schedulers"] = [
        schedulers.AnyBranchScheduler(
            # name="all", change_filter=pr_filter, builderNames=["runtests"]
            name="all",
            builderNames=["flake"],
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
    c["builders"] = [nix_builder_config(worker_config)]

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
