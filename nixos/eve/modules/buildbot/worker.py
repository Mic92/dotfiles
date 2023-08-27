#!/usr/bin/env python3

import multiprocessing
import os
import socket
from pathlib import Path

from buildbot_worker.bot import Worker
from twisted.application import service


def require_env(key: str) -> str:
    val = os.environ.get(key)
    assert val is not None, "val is not set"
    return val


PASSWD = Path(require_env("WORKER_PASSWORD_FILE")).read_text().strip("\r\n")
BUILDBOT_DIR = require_env("BUILDBOT_DIR")
MASTER_URL = require_env("MASTER_URL")


def setup_worker(application: service.Application, id: int) -> None:
    basedir = f"{BUILDBOT_DIR}-{id}"
    os.makedirs(basedir, mode=0o700, exist_ok=True)

    hostname = socket.gethostname()
    workername = f"{hostname}-{id}"
    keepalive = 600
    umask = None
    maxdelay = 300
    numcpus = None
    allow_shutdown = None

    s = Worker(
        None,
        None,
        workername,
        PASSWD,
        basedir,
        keepalive,
        connection_string=MASTER_URL,
        umask=umask,
        maxdelay=maxdelay,
        numcpus=numcpus,
        allow_shutdown=allow_shutdown,
    )
    # defaults to 4096, bump to 10MB for nix-eval-jobs
    s.bot.max_line_length = 10485760
    s.setServiceParent(application)


# note: this line is matched against to check that this is a worker
# directory; do not edit it.
application = service.Application("buildbot-worker")

for i in range(multiprocessing.cpu_count()):
    setup_worker(application, i)
