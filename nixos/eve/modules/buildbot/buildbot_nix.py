#!/usr/bin/env python3

import json
import os
import re
from pathlib import Path

from buildbot.steps.trigger import Trigger
from buildbot.plugins import util, steps
from typing import Any, Generator, Optional
from buildbot.process import buildstep, logobserver
from buildbot.process.properties import Properties
from twisted.internet import defer


class BuildTrigger(Trigger):
    def __init__(self, scheduler: str, jobs: list[dict[str, str]], **kwargs):
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
            attr = job.get("attr")
            drv_path = job.get("drvPath")
            error = job.get("error")
            out_path = job.get("outputs", {}).get("out")
            props = Properties()
            props.setProperty("virtual_builder_name", attr, "spawner")
            props.setProperty("virtual_builder_tags", "", "spawner")
            props.setProperty("attr", attr, "spawner")
            props.setProperty("drv_path", drv_path, "spawner")
            props.setProperty("out_path", out_path, "spawner")
            props.setProperty("error", error, "spawner")
            triggered_schedulers.append((sch, props))
        return triggered_schedulers


class NixEvalCommand(buildstep.ShellMixin, steps.BuildStep):
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


class NixBuildCommand(buildstep.ShellMixin, steps.BuildStep):
    def __init__(self, **kwargs):
        kwargs = self.setupShellMixin(kwargs)
        super().__init__(**kwargs)
        self.observer = logobserver.BufferLogObserver()
        self.addLogObserver("stdio", self.observer)

    @defer.inlineCallbacks
    def run(self) -> Generator[Any, object, Any]:
        error = self.getProperty("error")
        if error is not None:
            attr = self.getProperty("attr")
            # show eval error
            self.build.results = util.FAILURE
            log = yield self.addLog("nix-error")
            log.addStderr(f"{attr} failed to evaluate:\n{error}")
            return util.FAILURE

        # run `nix build`
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


class UpdateBuildOutput(steps.BuildStep):
    def __init__(self, branches: list[str], **kwargs):
        self.branches = branches
        super().__init__(**kwargs)

    def run(self) -> Generator[Any, object, Any]:
        props = self.build.getProperties()
        if props.getProperty("branch") not in self.branches:
            return util.SKIPPED
        attr = os.path.basename(props.getProperty("attr"))
        out_path = props.getProperty("out_path")
        # XXX don't hardcode this
        p = Path("/var/www/buildbot/nix-outputs/")
        os.makedirs(p, exist_ok=True)
        with open(p / attr, "w") as f:
            f.write(out_path)
        return util.SUCCESS


class MergePr(steps.ShellCommand):
    def __init__(
        self,
        github_token_secret: str,
        base_branches: list[str],
        owners: list[str],
        **kwargs: Any,
    ) -> None:
        self.github_token_secret = github_token_secret
        self.base_branches = base_branches
        self.rendered_token = None
        self.owners = owners
        super().__init__(**kwargs)

    @defer.inlineCallbacks
    def reconfigService(
        self,
        github_token_secret: str,
        base_branches: list[str],
        owners: list[str],
        **kwargs: Any,
    ) -> Generator[Any, object, Any]:
        self.rendered_token = yield self.renderSecrets(github_token_secret)
        self.base_branches = base_branches
        self.owners = owners
        super().reconfigService(**kwargs)

    @defer.inlineCallbacks
    def run(self) -> Generator[Any, object, Any]:
        props = self.build.getProperties()
        if props.getProperty("basename") not in self.base_branches:
            return util.SKIPPED
        if props.getProperty("event") not in ["pull_request"]:
            return util.SKIPPED
        if not any(owner in self.owners for owner in props.getProperty("owners")):
            return util.SKIPPED

        cmd = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)
        return util.SUCCESS


class CreatePr(steps.ShellCommand):
    def __init__(self, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self.addLogObserver(
            "stdio", logobserver.LineConsumerLogObserver(self.check_pr_exists)
        )

    def check_pr_exists(self):
        msg = re.compile(
            """a pull request for branch ".*" into branch ".*" already exists:"""
        )
        while True:
            _, line = yield
            if msg.search(line):
                self.pr_exists = True

    @defer.inlineCallbacks
    def run(self):
        self.pr_exists = False
        cmd = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)
        if self.pr_exists:
            return util.SKIPPED
        return cmd.results()


def nix_update_flake_config(
    worker_names: list[str], projectname: str, github_token_secret: str
) -> util.BuilderConfig:
    factory = util.BuildFactory()
    url_with_secret = util.Interpolate(
        f"https://git:%(secret:{github_token_secret})s@github.com/{projectname}"
    )
    factory.addStep(
        steps.Git(
            repourl=url_with_secret,
            method="clean",
            submodules=True,
            haltOnFailure=True,
        )
    )
    factory.addStep(
        steps.ShellCommand(
            name="Update flakes",
            env=dict(
                GIT_AUTHOR_NAME="buildbot",
                GIT_AUTHOR_EMAIL="buildbot@thalheim.io",
                GIT_COMMITTER_NAME="buildbot",
                GIT_COMMITTER_EMAIL="buildbot@thalheim.io",
            ),
            command=[
                "nix",
                "flake",
                "update",
                "--commit-lock-file",
                "--commit-lockfile-summary",
                "flake.lock: Update",
            ],
            haltOnFailure=True,
        )
    )
    factory.addStep(
        steps.ShellCommand(
            name="Force-Push to update_flake_lock branch",
            command=[
                "git",
                "push",
                "--force",
                "origin",
                "HEAD:refs/heads/update_flake_lock",
            ],
            haltOnFailure=True,
        )
    )
    factory.addStep(
        CreatePr(
            name="Create pull-request",
            env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
            command=[
                "gh",
                "pr",
                "create",
                "--repo",
                projectname,
                "--title",
                "flake.lock: Update",
                "--body",
                "Automatic buildbot update",
                "--head",
                "refs/heads/update_flake_lock",
                "--base",
                "master",
            ],
        )
    )
    return util.BuilderConfig(
        name="nix-update-flake",
        workernames=worker_names,
        factory=factory,
        properties=dict(virtual_builder_name="nix-update-flake"),
    )


def nix_eval_config(worker_names: list[str], github_token_secret: str) -> util.BuilderConfig:
    factory = util.BuildFactory()
    # check out the source
    factory.addStep(
        steps.GitHub(
            repourl=util.Property("repository"), method="clean", submodules=True
        )
    )

    factory.addStep(
        NixEvalCommand(
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
                # FIXME: don't hardcode this
                "/var/lib/buildbot-worker/gcroot",
                "--flake",
                ".#hydraJobs",
            ],
            haltOnFailure=True,
        )
    )
    # Merge flake-update pull requests if CI succeeds
    factory.addStep(
        MergePr(
            name="Merge pull-request",
            env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
            github_token_secret=util.Secret(github_token_secret),
            base_branches=["master"],
            owners=["mic92-buildbot"],
            command=[
                "gh",
                "pr",
                "merge",
                "--repo",
                util.Property("project"),
                "--rebase",
                util.Property("pullrequesturl"),
            ],
        )
    )

    return util.BuilderConfig(
        name="nix-eval",
        workernames=worker_names,
        factory=factory,
        properties=dict(virtual_builder_name="nix-eval"),
    )


def nix_build_config(
    worker_names: list[str], enable_cachix: bool
) -> util.BuilderConfig:
    factory = util.BuildFactory()
    factory.addStep(
        NixBuildCommand(
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
    if enable_cachix:
        factory.addStep(
            steps.ShellCommand(
                name="Upload cachix",
                env=dict(CACHIX_SIGNING_KEY=util.Secret("cachix-token")),
                command=[
                    "cachix",
                    "push",
                    util.Secret("cachix-name"),
                    util.Interpolate("result-%(prop:attr)s"),
                ],
            )
        )
    factory.addStep(UpdateBuildOutput(name="Update build output", branches=["master"]))
    return util.BuilderConfig(
        name="nix-build",
        workernames=worker_names,
        properties=[],
        collapseRequests=False,
        env={},
        factory=factory,
    )
