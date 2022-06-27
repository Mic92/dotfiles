#!/usr/bin/env python3

import os
import sys
import json

from buildbot.plugins import worker, util, steps, schedulers, reporters
from buildbot.steps.trigger import Trigger
from buildbot.process import buildstep, logobserver
from buildbot.process.properties import Properties, Interpolate
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


class BuildTrigger(Trigger):
    def __init__(self, scheduler: str, jobs: List[Dict[str, str]], **kwargs):
        if "name" not in kwargs:
            kwargs["name"] = "trigger"
        self.jobs = jobs
        self.config = None
        Trigger.__init__(
            self,
            waitForFinish=True,
            schedulerNames=[scheduler],
            haltOnFailure=True,
            flunkOnFailure=True,
            sourceStamps=[],
            alwaysUseLatest=False,
            updateSourceStamp=False,
            **kwargs,
        )

    def createTriggerProperties(self, props):
        return props

    def getSchedulersAndProperties(self):
        sch = self.schedulerNames[0]
        triggered_schedulers = []
        for job in self.jobs:
            attr = job["attr"]
            drv_path = job["drvPath"]
            props = Properties()
            props.setProperty("virtual_builder_name", attr, "spawner")
            props.setProperty("virtual_builder_tags", "", "spawner")
            props.setProperty("attr", attr, "spawner")
            props.setProperty("drv_path", drv_path, "spawner")
            triggered_schedulers.append((sch, props))
        return triggered_schedulers


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
            self.build.addStepsAfterCurrentStep(
                [BuildTrigger(scheduler="nix-build", name="nix-build", jobs=jobs)]
            )

        return result


def nix_eval_config(worker_names: List[str]) -> util.BuilderConfig:
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
            name="Eval flake",
            command=[
                "nix",
                "run",
                "github:nix-community/nix-eval-jobs",
                "--",
                "--workers",
                "8",
                "--gc-roots-dir",
                "/var/lib/buildbot-worker/gcroot",
                "--flake",
                ".#hydraJobs",
            ],
            haltOnFailure=True,
        )
    )

    return util.BuilderConfig(
        name="nix-eval", workernames=worker_names, factory=factory
    )


def nix_build_config(worker_names: List[str]) -> util.BuilderConfig:
    factory = util.BuildFactory()
    factory.addStep(
        steps.ShellCommand(
            env={},
            name="Build flake attr",
            command=[
                "nix",
                "build",
                "-L",
                "--out-link",
                util.Interpolate("result-%(prop:attr)s"),
                util.Property("drv_path"),
            ],
        )
    )
    return util.BuilderConfig(
        name="nix-build",
        workernames=worker_names,
        properties=[],
        collapseRequests=False,
        env={},
        factory=factory,
    )


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
    c["services"] = []
    c["services"].append(
        reporters.GitHubStatusPush(
            token=github_api_token,
            context=Interpolate("buildbot/%(prop:buildername)s"),
        ),
        reporters.IRC(
          host = "irc.r",
          nick = "buildbot",
          notify_events = [ 'finished', 'failure', 'success', 'exception', 'problem' ],
          channels = [{"channel": "#xxx"}],
          showBlameList = True,
          authz={'force': True},
        )
    )

    worker_config = json.loads(read_secret_file("github-workers"))

    c["workers"] = [worker.Worker(item["name"], item["pass"]) for item in worker_config]
    worker_names = [item["name"] for item in worker_config]
    c["builders"] = [nix_eval_config(worker_names), nix_build_config(worker_names)]

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
