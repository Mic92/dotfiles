#!/usr/bin/env python3

import os
import sys
import json

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
            props = Properties()
            props.setProperty("virtual_builder_name", attr, "spawner")
            props.setProperty("virtual_builder_tags", "", "spawner")
            props.setProperty("attr", attr, "spawner")
            props.setProperty("drv_path", drv_path, "spawner")
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


def nix_build_config(worker_names: List[str]) -> util.BuilderConfig:
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
    _irc_send(
        server=server,
        nick=username,
        sasl_password=password,
        channel=channel,
        port=port,
        messages=notifications,
        tls=parsed.scheme == "irc+tls",
    )


DEFAULT_MSG_TEMPLATE = """
{{status_detected}} on builder {{ buildername }} ({{ build_url }})
"""

class IrcNotifier(ReporterBase):
    def _generators(self) -> List[BuildStatusGenerator]:
        formatter = MessageFormatter(
            template_type="plain", template=DEFAULT_MSG_TEMPLATE
        )
        return [BuildStatusGenerator(message_formatter=formatter)]

    def checkConfig(self, url: str, generators=None):
        super().checkConfig(generators=self._generators())

    @defer.inlineCallbacks
    def reconfigService(self, url: str, generators=None) -> Generator[Any, object, Any]:
        self.url = url
        yield super().reconfigService(generators=self._generators())

    def sendMessage(self, reports: list):
        msgs = []
        for r in reports:
            subject = r["subject"]
            body = r["body"]
            msgs.append(f"{subject} {body}")
        irc_send(self.url, notifications=msgs)

    # @defer.inlineCallbacks
    # def sendMessage(self, reports):
    #    # Only use OAuth if basic auth has not been specified
    #    if not self.auth:
    #        request = yield self.oauthhttp.post("", data=_GET_TOKEN_DATA)
    #        if request.code != 200:
    #            content = yield request.content()
    #            log.msg(f"{request.code}: unable to authenticate to Bitbucket {content}")
    #            return
    #        token = (yield request.json())['access_token']
    #        self._http.updateHeaders({'Authorization': f'Bearer {token}'})

    #    build = reports[0]['builds'][0]
    #    if build['complete']:
    #        status = BITBUCKET_SUCCESSFUL if build['results'] == SUCCESS else BITBUCKET_FAILED
    #    else:
    #        status = BITBUCKET_INPROGRESS

    #    props = Properties.fromDict(build['properties'])
    #    props.master = self.master

    #    body = {
    #        'state': status,
    #        'key': (yield props.render(self.status_key)),
    #        'name': (yield props.render(self.status_name)),
    #        'description': reports[0]['subject'],
    #        'url': build['url']
    #    }

    #    for sourcestamp in build['buildset']['sourcestamps']:
    #        if not sourcestamp['repository']:
    #            log.msg(f"Empty repository URL for Bitbucket status {body}")
    #            continue
    #        owner, repo = self.get_owner_and_repo(sourcestamp['repository'])

    #        endpoint = (owner, repo, 'commit', sourcestamp['revision'], 'statuses', 'build')
    #        bitbucket_uri = f"/{'/'.join(endpoint)}"

    #        if self.debug:
    #            log.msg(f"Bitbucket status {bitbucket_uri} {body}")

    #        response = yield self._http.post(bitbucket_uri, json=body)
    #        if response.code not in (200, 201):
    #            content = yield response.content()
    #            log.msg(f"{response.code}: unable to upload Bitbucket status {content}")

    # def setServiceParent(self, parent) -> None:
    #    StatusReceiverMultiService.setServiceParent(self, parent)
    #    self.master_status = self.parent
    #    self.master_status.subscribe(self)
    #    self.master = self.master_status.master

    # def disownServiceParent(self) -> None:
    #    self.master_status.unsubscribe(self)
    #    self.master_status = None
    #    for w in self.watched:
    #        w.unsubscribe(self)
    #    return StatusReceiverMultiService.disownServiceParent(self)

    # def builderAdded(self, name, builder) -> "IrcPush":
    #    return self  # subscribe to this builder

    # def buildFinished(self, builderName: str, build, result) -> None:
    #    assert self.master_status is not None
    #    url = self.master_status.getURLForThing(build)

    #    message = "%s - %s - <%s>" % \
    #        (builderName, Results[result].upper(), url)
    #    irc_send(self.url,  notifications=[message])


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
        IrcNotifier("irc://buildbot@irc.r:6667/#xxx")
        #reporters.IRC(
        #    host="irc.r",
        #    nick="buildbot|mic92",
        #    notify_events=["finished", "failure", "success", "exception", "problem"],
        #    noticeOnChannel=True,
        #    channels=[{"channel": "#xxx"}],
        #    # showBlameList = True,
        #    authz={"force": True},
        #),
    ]

    worker_config = json.loads(read_secret_file("github-workers"))

    # credentials = os.environ.get("CREDENTIALS_DIRECTORY")
    # assert not credentials, "No CREDENTIALS_DIRECTORY has been provided"

    # c['secretsProviders'] = [secrets.SecretInAFile(dirname=credentials)]
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
