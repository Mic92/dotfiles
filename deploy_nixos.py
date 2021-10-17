#!/usr/bin/env python3

import os
from contextlib import contextmanager
from typing import List, Dict, Tuple, IO, Iterator, Optional, Callable, Any
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
        port: int = 22,
        forward_agent: bool = False,
        command_prefix: Optional[str] = None,
        meta: Dict[str, Any] = {},
    ) -> None:
        self.host = host
        self.user = user
        self.port = port
        if command_prefix:
            self.command_prefix = command_prefix
        else:
            self.command_prefix = host
        self.forward_agent = forward_agent
        self.meta = meta

    def _prefix_output(self, fd: IO[str]) -> None:
        for line in fd:
            print(f"[{self.command_prefix}] {line}", end="")

    def run_local(self, cmd: str) -> int:
        print(f"[{self.command_prefix}] {cmd}")
        with pipe() as (read_fd, write_fd):
            with subprocess.Popen(
                cmd, text=True, shell=True, stdout=write_fd, stderr=write_fd
            ) as p:
                write_fd.close()
                self._prefix_output(read_fd)
                return p.wait()

    def run(self, cmd: str) -> int:
        print(f"[{self.command_prefix}] {cmd}")
        with pipe() as (read_fd, write_fd):
            ssh_opts = ["-A"] if self.forward_agent else []
            with subprocess.Popen(
                ["ssh", f"{self.user}@{self.host}", "-p", str(self.port)]
                + ssh_opts
                + ["--", cmd],
                stdout=write_fd,
                stderr=write_fd,
                text=True,
            ) as p:
                write_fd.close()
                self._prefix_output(read_fd)
                return p.wait()


class DeployGroup:
    def __init__(self, hosts: List[DeployHost]) -> None:
        self.hosts = hosts

    def _run_local(self, cmd: str, host: DeployHost, queue: Queue) -> None:
        queue.put((host, host.run_local(cmd)))

    def _run_remote(self, cmd: str, host: DeployHost, queue: Queue) -> None:
        queue.put((host, host.run(cmd)))

    def _run(
        self, cmd: str, local: bool = False
    ) -> Dict[DeployHost, int]:
        queue: Queue[Tuple[DeployHost, int]] = Queue()
        threads = []
        for host in self.hosts:
            fn = self._run_local if local else self._run_remote
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
                args=(host,),
            )
            threads.append(thread)

        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()
