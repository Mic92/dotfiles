#!/usr/bin/env python3

import json
import os
import re
import multiprocessing
from pathlib import Path
import uuid
from collections import defaultdict

from buildbot.steps.trigger import Trigger
from buildbot.plugins import util, steps
from typing import Any, Generator, List
from buildbot.process import buildstep, logobserver
from buildbot.process.properties import Properties
from twisted.internet import defer
from buildbot.process.results import ALL_RESULTS
from buildbot.process.results import statusToString


class BuildTrigger(Trigger):
    """
    Dynamic trigger that creates a build for every attribute.
    """

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
        build_props = self.build.getProperties()
        repo_name = build_props.getProperty("github.repository.full_name")

        sch = self.schedulerNames[0]
        triggered_schedulers = []
        for job in self.jobs:
            attr = job.get("attr", "eval-error")
            name = attr
            if repo_name is not None:
                name = f"{repo_name}: {name}"
            drv_path = job.get("drvPath")
            error = job.get("error")
            out_path = job.get("outputs", {}).get("out")
            props = Properties()
            props.setProperty("virtual_builder_name", name, "spawner")
            props.setProperty("virtual_builder_tags", "", "spawner")
            props.setProperty("attr", attr, "spawner")
            props.setProperty("drv_path", drv_path, "spawner")
            props.setProperty("out_path", out_path, "spawner")
            # we use this to identify builds when running a retry
            props.setProperty("build_uuid", str(uuid.uuid4()), "spawner")
            props.setProperty("error", error, "spawner")
            triggered_schedulers.append((sch, props))
        return triggered_schedulers

    def getCurrentSummary(self):
        """
        The original build trigger will the generic builder name `nix-build` in this case, which is not helpful
        """
        if not self.triggeredNames:
            return {"step": "running"}
        summary = []
        if self._result_list:
            for status in ALL_RESULTS:
                count = self._result_list.count(status)
                if count:
                    summary.append(
                        f"{self._result_list.count(status)} {statusToString(status, count)}"
                    )
        return {"step": f"({', '.join(summary)})"}


class NixEvalCommand(buildstep.ShellMixin, steps.BuildStep):
    """
    Parses the output of `nix-eval-jobs` and triggers a `nix-build` build for
    every attribute.
    """

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
                    job = json.loads(line)
                    jobs.append(job)
            self.build.addStepsAfterCurrentStep(
                [BuildTrigger(scheduler="nix-build", name="nix-build", jobs=jobs)]
            )

        return result


# FIXME this leaks memory... but probably not enough that we care
class RetryCounter:
    def __init__(self, retries: int) -> None:
        self.builds: dict[uuid.UUID, int] = defaultdict(lambda: retries)

    def retry_build(self, id: uuid.UUID) -> int:
        retries = self.builds[id]
        if retries > 1:
            self.builds[id] = retries - 1
            return retries
        else:
            return 0


# For now we limit this to two. Often this allows us to make the error log
# shorter because we won't see the logs for all previous succeeded builds
RETRY_COUNTER = RetryCounter(retries=2)


class NixBuildCommand(buildstep.ShellMixin, steps.BuildStep):
    """
    Builds a nix derivation if evaluation was successful,
    otherwise this shows the evaluation error.
    """

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
            log = yield self.addLog("nix_error")
            log.addStderr(f"{attr} failed to evaluate:\n{error}")
            return util.FAILURE

        # run `nix build`
        cmd = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)

        res = cmd.results()
        if res == util.FAILURE:
            retries = RETRY_COUNTER.retry_build(self.getProperty("build_uuid"))
            if retries > 0:
                return util.RETRY
        return res


class UpdateBuildOutput(steps.BuildStep):
    """
    Updates store paths in a public www directory.
    This is useful to prefetch updates without having to evaluate
    on the target machine.
    """

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
    """
    Merge a pull request for specified branches and pull request owners
    """

    def __init__(
        self,
        base_branches: list[str],
        owners: list[str],
        **kwargs: Any,
    ) -> None:
        super().__init__(**kwargs)
        self.base_branches = base_branches
        self.owners = owners
        self.observer = logobserver.BufferLogObserver()
        self.addLogObserver("stdio", self.observer)

    @defer.inlineCallbacks
    def reconfigService(
        self,
        base_branches: list[str],
        owners: list[str],
        **kwargs: Any,
    ) -> Generator[Any, object, Any]:
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
        return cmd.results()


class CreatePr(steps.ShellCommand):
    """
    Creates a pull request if none exists
    """

    def __init__(self, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self.addLogObserver(
            "stdio", logobserver.LineConsumerLogObserver(self.check_pr_exists)
        )

    def check_pr_exists(self):
        ignores = [
            re.compile(
                """a pull request for branch ".*" into branch ".*" already exists:"""
            ),
            re.compile("No commits between .* and .*"),
        ]
        while True:
            _, line = yield
            if any(ignore.search(line) is not None for ignore in ignores):
                self.skipped = True

    @defer.inlineCallbacks
    def run(self):
        self.skipped = False
        cmd = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)
        if self.skipped:
            return util.SKIPPED
        return cmd.results()


def nix_update_flake_config(
    worker_names: list[str],
    projectname: str,
    github_token_secret: str,
    github_bot_user: str
) -> util.BuilderConfig:
    """
    Updates the flake an opens a PR for it.
    """
    factory = util.BuildFactory()
    url_with_secret = util.Interpolate(
        f"https://git:%(secret:{github_token_secret})s@github.com/{projectname}"
    )
    factory.addStep(
        steps.Git(
            repourl=url_with_secret,
            alwaysUseLatest=True,
            method="clean",
            submodules=True,
            haltOnFailure=True,
        )
    )
    factory.addStep(
        steps.ShellCommand(
            name="Update flakes",
            env=dict(
                GIT_AUTHOR_NAME=github_bot_user,
                GIT_AUTHOR_EMAIL=f"{github_bot_user}@users.noreply.github.com",
                GIT_COMMITTER_NAME=github_bot_user,
                GIT_COMMITTER_EMAIL=f"{github_bot_user}@users.noreply.github.com",
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


def nix_eval_config(
    worker_names: list[str], github_token_secret: str, automerge_users: List[str] = []
) -> util.BuilderConfig:
    """
    Uses nix-eval-jobs to evaluate hydraJobs from flake.nix in parallel.
    For each evaluated attribute a new build pipeline is started.
    If all builds succeed and the build was for a PR opened by the flake update bot,
    this PR is merged.
    """
    factory = util.BuildFactory()
    # check out the source
    url_with_secret = util.Interpolate(
        f"https://git:%(secret:{github_token_secret})s@github.com/%(prop:project)s"
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
        NixEvalCommand(
            env={},
            name="Eval flake",
            command=[
                "nix",
                "run",
                "github:nix-community/nix-eval-jobs",
                "--",
                "--workers",
                multiprocessing.cpu_count(),
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
    if len(automerge_users) > 0:
        factory.addStep(
            MergePr(
                name="Merge pull-request",
                env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
                base_branches=["master"],
                owners=automerge_users,
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
    worker_names: list[str],
    has_cachix_auth_token: bool = False,
    has_cachix_signing_key: bool = False,
) -> util.BuilderConfig:
    """
    Builds one nix flake attribute.
    """
    factory = util.BuildFactory()
    factory.addStep(
        NixBuildCommand(
            env={},
            name="Build flake attr",
            command=[
                "nix",
                "build",
                "--option",
                "keep-going",
                "true",
                "-L",
                "--out-link",
                util.Interpolate("result-%(prop:attr)s"),
                util.Property("drv_path"),
            ],
            haltOnFailure=True,
        )
    )
    if has_cachix_auth_token or has_cachix_signing_key:
        if has_cachix_signing_key:
            env = dict(CACHIX_SIGNING_KEY=util.Secret("cachix-signing-key"))
        else:
            env = dict(CACHIX_AUTH_TOKEN=util.Secret("cachix-auth-token"))
        factory.addStep(
            steps.ShellCommand(
                name="Upload cachix",
                env=env,
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
