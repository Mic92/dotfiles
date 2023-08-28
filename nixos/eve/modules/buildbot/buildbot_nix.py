#!/usr/bin/env python3

import json
import multiprocessing
import os
import uuid
from collections import defaultdict
from pathlib import Path
from typing import Any, Generator, List

from buildbot.plugins import steps, util
from buildbot.process import buildstep, logobserver, remotecommand
from buildbot.process.log import Log
from buildbot.process.properties import Properties
from buildbot.process.results import ALL_RESULTS, statusToString
from buildbot.steps.trigger import Trigger
from github_projects import GithubProject
from twisted.internet import defer


class BuildTrigger(Trigger):
    """
    Dynamic trigger that creates a build for every attribute.
    """

    def __init__(
        self, scheduler: str, jobs: list[dict[str, Any]], **kwargs: Any
    ) -> None:
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

    def createTriggerProperties(self, props: Any) -> Any:
        return props

    def getSchedulersAndProperties(self) -> list[tuple[str, Properties]]:
        build_props = self.build.getProperties()
        repo_name = build_props.getProperty(
            "github.base.repo.full_name",
            build_props.getProperty("github.repository.full_name"),
        )
        project_id = repo_name.replace("/", "-")
        source = f"nix-eval-{project_id}"

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

            build_props.setProperty(f"{attr}-out_path", out_path, source)
            build_props.setProperty(f"{attr}-drv_path", drv_path, source)

            props = Properties()
            props.setProperty("virtual_builder_name", name, source)
            props.setProperty("virtual_builder_tags", "", source)
            props.setProperty("attr", attr, source)
            props.setProperty("drv_path", drv_path, source)
            props.setProperty("out_path", out_path, source)
            # we use this to identify builds when running a retry
            props.setProperty("build_uuid", str(uuid.uuid4()), source)
            props.setProperty("error", error, source)
            triggered_schedulers.append((sch, props))
        return triggered_schedulers

    def getCurrentSummary(self) -> dict[str, str]:
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

    def __init__(self, **kwargs: Any) -> None:
        kwargs = self.setupShellMixin(kwargs)
        super().__init__(**kwargs)
        self.observer = logobserver.BufferLogObserver()
        self.addLogObserver("stdio", self.observer)

    @defer.inlineCallbacks
    def run(self) -> Generator[Any, object, Any]:
        # run nix-instanstiate to generate the dict of stages
        cmd: remotecommand.RemoteCommand = yield self.makeRemoteShellCommand()
        yield self.runCommand(cmd)

        # if the command passes extract the list of stages
        result = cmd.results()
        if result == util.SUCCESS:
            # create a ShellCommand for each stage and add them to the build
            jobs = []

            for line in self.observer.getStdout().split("\n"):
                if line != "":
                    try:
                        job = json.loads(line)
                    except json.JSONDecodeError as e:
                        raise Exception(f"Failed to parse line: {line}") from e
                    jobs.append(job)
            build_props = self.build.getProperties()
            repo_name = build_props.getProperty(
                "github.base.repo.full_name",
                build_props.getProperty("github.repository.full_name"),
            )
            project_id = repo_name.replace("/", "-")
            builder = f"{project_id}-nix-build"

            self.build.addStepsAfterCurrentStep(
                [BuildTrigger(scheduler=builder, name=builder, jobs=jobs)]
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

    def __init__(self, **kwargs: Any) -> None:
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
            log: Log = yield self.addLog("nix_error")
            log.addStderr(f"{attr} failed to evaluate:\n{error}")
            return util.FAILURE

        # run `nix build`
        cmd: remotecommand.RemoteCommand = yield self.makeRemoteShellCommand()
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

    def __init__(self, **kwargs: Any) -> None:
        super().__init__(**kwargs)

    def run(self) -> Generator[Any, object, Any]:
        props = self.build.getProperties()
        if props.getProperty("branch") != props.getProperty(
            "github.repository.default_branch"
        ):
            return util.SKIPPED
        attr = os.path.basename(props.getProperty("attr"))
        out_path = props.getProperty("out_path")
        # XXX don't hardcode this
        p = Path("/var/www/buildbot/nix-outputs/")
        os.makedirs(p, exist_ok=True)
        (p / attr).write_text(out_path)
        return util.SUCCESS


def nix_update_flake_config(
    project: GithubProject,
    worker_names: list[str],
    github_token_secret: str,
    github_bot_user: str,
) -> util.BuilderConfig:
    """
    Updates the flake an opens a PR for it.
    """
    factory = util.BuildFactory()
    url_with_secret = util.Interpolate(
        f"https://git:%(secret:{github_token_secret})s@github.com/{project.name}"
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
        steps.SetPropertyFromCommand(
            env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
            command=[
                "gh",
                "pr",
                "view",
                "--json",
                "state",
                "--template",
                "{{.state}}",
                "update_flake_lock",
            ],
            decodeRC={0: "SUCCESS", 1: "SUCCESS"},
            property="has_pr",
        )
    )
    factory.addStep(
        steps.ShellCommand(
            name="Create pull-request",
            env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
            command=[
                "gh",
                "pr",
                "create",
                "--repo",
                project.name,
                "--title",
                "flake.lock: Update",
                "--body",
                "Automatic buildbot update",
                "--head",
                "refs/heads/update_flake_lock",
                "--base",
                project.default_branch,
            ],
            doStepIf=util.Interpolate("has_pr") != "OPEN",
        )
    )
    return util.BuilderConfig(
        name=f"{project.id}-update-flake",
        project=project.name,
        workernames=worker_names,
        factory=factory,
        properties=dict(virtual_builder_name="{project.name}/update-flake"),
    )


def nix_eval_config(
    project: GithubProject,
    worker_names: list[str],
    github_token_secret: str,
    automerge_users: List[str] = [],
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
            name="evaluate flake",
            command=[
                "nix",
                "run",
                "--option",
                "accept-flake-config",
                "true",
                "github:nix-community/nix-eval-jobs",
                "--",
                "--workers",
                multiprocessing.cpu_count(),
                "--option",
                "accept-flake-config",
                "true",
                "--gc-roots-dir",
                # FIXME: don't hardcode this
                "/var/lib/buildbot-worker/gcroot",
                "--force-recurse",
                "--flake",
                ".#checks",
            ],
            haltOnFailure=True,
        )
    )
    if len(automerge_users) > 0:

        def check_auto_merge(step: steps.BuildStep) -> bool:
            print("Checking if we should merge")
            props = step.build.getProperties()
            if props.getProperty("event") != "pull_request":
                print("Not a pull request")
                return False
            if props.getProperty(
                "github.repository.default_branch"
            ) != props.getProperty("branch"):
                print("Not on default branch")
                return False
            if not any(
                owner in automerge_users for owner in props.getProperty("owners")
            ):
                print(
                    f"PR opened by {step.getProperty('owner')} not in {automerge_users}"
                )
                return False
            return True

        factory.addStep(
            steps.ShellCommand(
                name="Merge pull-request",
                env=dict(GITHUB_TOKEN=util.Secret(github_token_secret)),
                command=[
                    "gh",
                    "pr",
                    "merge",
                    "--repo",
                    util.Property("project"),
                    "--rebase",
                    util.Property("pullrequesturl"),
                ],
                doStepIf=check_auto_merge,
            )
        )

    return util.BuilderConfig(
        name=f"{project.id}-nix-eval",
        workernames=worker_names,
        project=project.name,
        factory=factory,
        properties=dict(virtual_builder_name=f"{project.name}/nix-eval"),
    )


def nix_build_config(
    project: GithubProject,
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
                "-L",
                "--option",
                "keep-going",
                "true",
                "--accept-flake-config",
                "--out-link",
                util.Interpolate("result-%(prop:attr)s"),
                util.Interpolate("%(prop:drv_path)s^*"),
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
    factory.addStep(
        steps.ShellCommand(
            name="Register gcroot",
            command=[
                "nix-store",
                "--add-root",
                # FIXME: cleanup old build attributes
                util.Interpolate(
                    "/nix/var/nix/profiles/per-user/buildbot-worker/result-%(prop:attr)s"
                ),
                "-r",
                util.Property("out_path"),
            ],
            doStepIf=util.Interpolate("branch")
            == util.Interpolate("github.repository.default_branch"),
        )
    )
    factory.addStep(
        steps.ShellCommand(
            name="Delete temporary gcroots",
            command=["rm", "-f", util.Interpolate("result-%(prop:attr)s")],
        )
    )
    factory.addStep(UpdateBuildOutput(name="Update build output"))
    return util.BuilderConfig(
        name=f"{project.id}-nix-build",
        project=project.name,
        workernames=worker_names,
        collapseRequests=False,
        env={},
        factory=factory,
    )
