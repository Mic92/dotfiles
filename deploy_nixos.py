#!/usr/bin/env python3

import os
from contextlib import contextmanager
from string import Template
from typing import List, Dict, Tuple, IO, Iterator, Optional, Callable
from threading import Thread
from queue import Queue
import subprocess

@contextmanager
def pipe() -> Iterator[Tuple[IO[str], IO[str]]]:
    (pipe_r, pipe_w) = os.pipe()
    read_end = os.fdopen(pipe_r, "r")
    write_end = os.fdopen(pipe_w, "w")
    try:
        yield (read_end, write_end)
    finally:
        read_end.close()
        write_end.close()


class DeployHost:
    def __init__(
        self,
        host: str,
        user: str = "root",
        target_host: str = "localhost",
        port: int = 22,
        forward_agent: bool = False,
        flake_attr: str = "",
    ):
        self.host = host
        self.user = user
        self.target_host = target_host
        self.port = port
        self.forward_agent = forward_agent
        self.flake_attr = flake_attr


class DeployGroup:
    def __init__(self, hosts: List[DeployHost]) -> None:
        self.hosts = hosts

    def _substitute_vars(self, cmd: str, host: DeployHost) -> str:
        t = Template(cmd)
        variables = dict(
            SSH_HOST=host.host,
            SSH_USER=host.user,
            SSH_PORT=host.port,
            TARGET_HOST=host.target_host,
            FLAKE_ATTR=host.flake_attr,
        )
        return t.safe_substitute(variables)

    def _prefix_output(self, fd: IO[str], host: DeployHost) -> None:
        target = host.target_host if host.target_host != "localhost" else host.host
        for line in fd:
            print(f"[{target}] {line}", end="")

    def _local_command(self, cmd: str, host: DeployHost, queue: Queue) -> None:
        cmd = self._substitute_vars(cmd, host)
        print(f"[{host.host}] {cmd}")
        with pipe() as (read_fd, write_fd):
            with subprocess.Popen(
                cmd, text=True, shell=True, stdout=write_fd, stderr=write_fd
            ) as p:
                write_fd.close()
                self._prefix_output(read_fd, host)
                ret = p.wait()
                queue.put((host, ret))

    def _ssh_command(self, cmd: str, host: DeployHost, queue: Queue) -> None:
        cmd = self._substitute_vars(cmd, host)
        print(f"[{host.host}] {cmd}")
        with pipe() as (read_fd, write_fd):
            ssh_opts = ["-A"] if host.forward_agent else []
            with subprocess.Popen(
                ["ssh", f"{host.user}@{host.host}", "-p", str(host.port)]
                + ssh_opts
                + ["--", cmd],
                stdout=write_fd,
                stderr=write_fd,
                text=True,
            ) as p:
                write_fd.close()
                self._prefix_output(read_fd, host)
                res = p.wait()
                queue.put((host, res))

    def _run(
        self, cmd: str, local: bool = False
    ) -> Dict[DeployHost, subprocess.CompletedProcess]:
        queue: Queue[Tuple[DeployHost, subprocess.CompletedProcess]] = Queue()
        threads = []
        for host in self.hosts:
            fn = self._local_command if local else self._ssh_command
            thread = Thread(
                target=fn,
                kwargs=dict(queue=queue, cmd=cmd, host=host),
            )
            threads.append(thread)

        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()

        results = {}
        while not queue.empty():
            host, result = queue.get(block=False)
            results[host] = result
        return results

    def run(self, cmd: str) -> None:
        self._run(cmd)

    def run_local(self, cmd: str) -> None:
        self._run(cmd, local=True)

    def run_function(self, func: Callable) -> None:
        threads = []
        for host in self.hosts:
            thread = Thread(
                target=func,
                kwargs=dict(host=host),
            )
            threads.append(thread)

        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()
