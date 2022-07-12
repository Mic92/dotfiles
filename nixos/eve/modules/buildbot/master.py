#!/usr/bin/env python3

import os
import sys
import json
import threading

from buildbot.plugins import worker, util, steps, schedulers, reporters, secrets
from buildbot.steps.trigger import Trigger
from buildbot.reporters.base import ReporterBase
from buildbot.process import buildstep, logobserver
from buildbot.process.properties import Properties, Interpolate
from buildbot.reporters.generators.build import BuildStatusGenerator
from buildbot.reporters.message import MessageFormatter
from typing import Generator, Optional, List
from pathlib import Path
from typing import Any, Dict, List
from twisted.internet import defer
import ssl
import socket
import base64
import re
from urllib.parse import urlparse


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
    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def run(self) -> Generator[Any, object, Any]:
        properties = self.build.getProperties()
        props = {}
        for key, value, _ in properties.asList():
            props[key] = value
        attr = os.path.basename(props["attr"])
        out_path = props["out_path"]
        # XXX don't hardcode this
        p = Path("/var/www/buildbot/nix-outputs/")
        os.makedirs(p, exist_ok=True)
        with open(p / attr, "w") as f:
            f.write(out_path)
        return util.SUCCESS


def nix_eval_config(worker_names: List[str]) -> util.BuilderConfig:
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
                # XXX don't hardcode this
                "/var/lib/buildbot-worker/gcroot",
                "--flake",
                ".#hydraJobs",
            ],
            haltOnFailure=True,
        )
    )

    return util.BuilderConfig(
        name="nix-eval",
        workernames=worker_names,
        factory=factory,
        properties=dict(virtual_builder_name="nix-eval"),
    )


def nix_build_config(
    worker_names: List[str], enable_cachix: bool
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
    factory.addStep(UpdateBuildOutput(name="Update build output"))
    return util.BuilderConfig(
        name="nix-build",
        workernames=worker_names,
        properties=[],
        collapseRequests=False,
        env={},
        factory=factory,
    )


DEBUG = False


def _irc_send(
    server: str,
    nick: str,
    channel: str,
    sasl_password: Optional[str] = None,
    server_password: Optional[str] = None,
    tls: bool = True,
    port: int = 6697,
    messages: List[str] = [],
) -> None:
    if not messages:
        return

    # don't give a shit about legacy ip
    sock = socket.socket(family=socket.AF_INET6)
    if tls:
        sock = ssl.wrap_socket(
            sock, cert_reqs=ssl.CERT_NONE, ssl_version=ssl.PROTOCOL_TLSv1_2
        )

    def _send(command: str) -> int:
        if DEBUG:
            print(command)
        return sock.send((f"{command}\r\n").encode())

    def _pong(ping: str):
        if ping.startswith("PING"):
            sock.send(ping.replace("PING", "PONG").encode("ascii"))

    recv_file = sock.makefile(mode="r")

    print(f"connect {server}:{port}")
    sock.connect((server, port))
    if server_password:
        _send(f"PASS {server_password}")
    _send(f"USER {nick} 0 * :{nick}")
    _send(f"NICK {nick}")
    for line in recv_file.readline():
        if re.match(r"^:[^ ]* (MODE|221|376|422) ", line):
            break
        else:
            _pong(line)

    if sasl_password:
        _send("CAP REQ :sasl")
        _send("AUTHENTICATE PLAIN")
        auth = base64.encodebytes(f"{nick}\0{nick}\0{sasl_password}".encode("ascii"))
        _send(f"AUTHENTICATE {auth.decode('ascii')}")
        _send("CAP END")
    _send(f"JOIN :{channel}")

    for m in messages:
        _send(f"PRIVMSG {channel} :{m}")

    _send("INFO")
    for line in recv_file:
        if DEBUG:
            print(line, end="")
        # Assume INFO reply means we are done
        if "End of /INFO" in line:
            break
        else:
            _pong(line)

    sock.send(b"QUIT")
    print("disconnect")
    sock.close()


def irc_send(
    url: str, notifications: List[str], password: Optional[str] = None
) -> None:
    parsed = urlparse(f"{url}")
    username = parsed.username or "prometheus"
    server = parsed.hostname or "chat.freenode.net"
    if parsed.fragment != "":
        channel = f"#{parsed.fragment}"
    else:
        channel = "#krebs-announce"
    port = parsed.port or 6697
    if not password:
        password = parsed.password
    if len(notifications) == 0:
        return
    # put this in a thread to not block buildbot
    t = threading.Thread(
        target=_irc_send,
        kwargs=dict(
            server=server,
            nick=username,
            sasl_password=password,
            channel=channel,
            port=port,
            messages=notifications,
            tls=parsed.scheme == "irc+tls",
        ),
    )
    t.start()


subject_template = """\
{{ '☠' if result_names[results] == 'failure' else '☺' if result_names[results] == 'success' else '☝' }} \
{{ build['properties'].get('project', ['whole buildset'])[0] if is_buildset else buildername }} \
- \
{{ build['state_string'] }} \
{{ '(%s)' % (build['properties']['branch'][0] if (build['properties']['branch'] and build['properties']['branch'][0]) else build['properties'].get('got_revision', ['(unknown revision)'])[0]) }} \
({{ build_url }})
"""  # # noqa pylint: disable=line-too-long


class NotifyFailedBuilds(ReporterBase):
    def _generators(self) -> List[BuildStatusGenerator]:
        formatter = MessageFormatter(template_type="plain", subject=subject_template)
        return [BuildStatusGenerator(message_formatter=formatter)]

    def checkConfig(self, url: str):
        super().checkConfig(generators=self._generators())

    @defer.inlineCallbacks
    def reconfigService(self, url: str) -> Generator[Any, object, Any]:
        self.url = url
        yield super().reconfigService(generators=self._generators())

    def sendMessage(self, reports: list):
        msgs = []
        for r in reports:
            if r["builds"][0]["state_string"] != "build successful":
                msgs.append(r["subject"])
        irc_send(self.url, notifications=msgs)


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
        NotifyFailedBuilds("irc://buildbot|mic92@irc.r:6667/#spam"),
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
