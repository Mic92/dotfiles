#!/usr/bin/env python3
"""
Radicle GitHub Sync - Socket-activated sync from GitHub to Radicle.

This module provides two commands:
- request: Send a sync request via Unix socket (called via SSH)
- daemon: Listen on socket and process requests (systemd socket-activated)
"""

import argparse
import contextlib
import json
import os
import socket
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

SOCKET_PATH = "/run/radicle-sync/sync.sock"
REPOS_DIR = Path("/var/lib/radicle-sync/repos")
RADICLE_STORAGE = Path("/var/lib/radicle/storage")


@dataclass
class SyncRequest:
    repo_id: str
    github_url: str
    branch: str

    def to_json(self) -> str:
        return json.dumps(
            {
                "repo_id": self.repo_id,
                "github_url": self.github_url,
                "branch": self.branch,
            }
        )

    @classmethod
    def from_json(cls, data: str) -> "SyncRequest":
        d = json.loads(data)
        return cls(
            repo_id=d["repo_id"],
            github_url=d["github_url"],
            branch=d["branch"],
        )


def send_request(repo_id: str, github_url: str, branch: str) -> int:
    """Send a sync request to the daemon via Unix socket."""
    # Verify Radicle repo exists
    radicle_storage = RADICLE_STORAGE / repo_id
    if not radicle_storage.is_dir():
        print(f"Repository {repo_id} not found in Radicle storage", file=sys.stderr)
        return 1

    request = SyncRequest(repo_id=repo_id, github_url=github_url, branch=branch)

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(SOCKET_PATH)
        sock.sendall(request.to_json().encode() + b"\n")

        # Read response
        response = b""
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            response += chunk

        print(response.decode(), end="")
        sock.close()
    except (OSError, ConnectionRefusedError) as e:
        print(f"Failed to connect to sync daemon: {e}", file=sys.stderr)
        return 1
    else:
        return 0


def run_git(*args: str, cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    """Run a git command."""
    return subprocess.run(
        ["git", *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )


def process_request(request: SyncRequest) -> tuple[bool, str]:
    """Process a single sync request. Returns (success, message)."""
    repo_id = request.repo_id
    github_url = request.github_url
    branch = request.branch

    messages = [f"Processing: {repo_id} from {github_url} ({branch})"]

    radicle_storage = RADICLE_STORAGE / repo_id
    if not radicle_storage.is_dir():
        return False, f"Repository {repo_id} not found in Radicle storage"

    repo_work = REPOS_DIR / repo_id
    REPOS_DIR.mkdir(parents=True, exist_ok=True)

    try:
        # Create or update persistent work repo
        if not repo_work.is_dir():
            messages.append(f"Creating persistent work repo for {repo_id}")
            run_git("clone", str(radicle_storage), str(repo_work))
            run_git("remote", "add", "github", github_url, cwd=repo_work)
        else:
            # Ensure remotes are configured correctly
            with contextlib.suppress(subprocess.CalledProcessError):
                run_git(
                    "remote", "set-url", "origin", str(radicle_storage), cwd=repo_work
                )
            try:
                run_git("remote", "set-url", "github", github_url, cwd=repo_work)
            except subprocess.CalledProcessError:
                run_git("remote", "add", "github", github_url, cwd=repo_work)

        # Fetch from GitHub
        messages.append(f"Fetching {branch} from GitHub...")
        run_git("fetch", "github", branch, cwd=repo_work)
        run_git("checkout", "FETCH_HEAD", cwd=repo_work)

        # Push to Radicle via rad:// (this triggers signing)
        messages.append("Pushing to Radicle...")
        run_git(
            "push",
            f"rad://{repo_id}",
            f"HEAD:refs/heads/{branch}",
            "--force",
            cwd=repo_work,
        )

        # Announce to network
        messages.append("Announcing to network...")
        subprocess.run(
            ["rad", "sync", "--announce", f"rad:{repo_id}"],
            capture_output=True,
            text=True,
            check=False,  # Don't fail if announce fails
        )

        messages.append(f"Successfully synced {repo_id}")
        return True, "\n".join(messages)

    except subprocess.CalledProcessError as e:
        error_msg = f"Failed to sync {repo_id}: {e}"
        if e.stdout:
            error_msg += f"\nstdout: {e.stdout}"
        if e.stderr:
            error_msg += f"\nstderr: {e.stderr}"
        messages.append(error_msg)
        return False, "\n".join(messages)


def get_systemd_socket() -> socket.socket | None:
    """Get socket from systemd socket activation."""
    # Check for systemd socket activation
    listen_fds = os.environ.get("LISTEN_FDS")
    if listen_fds and int(listen_fds) > 0:
        # File descriptor 3 is the first passed socket
        # SD_LISTEN_FDS_START = 3
        return socket.fromfd(3, socket.AF_UNIX, socket.SOCK_STREAM)
    return None


def run_daemon() -> int:
    """Run the sync daemon (socket-activated)."""
    sock = get_systemd_socket()
    if sock is None:
        print("No systemd socket provided, creating our own", file=sys.stderr)
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock_path = Path(SOCKET_PATH)
        sock_path.parent.mkdir(parents=True, exist_ok=True)
        sock_path.unlink(missing_ok=True)
        sock.bind(SOCKET_PATH)
        sock.listen(5)

    # Set a timeout so we exit after being idle
    sock.settimeout(30.0)

    print("Radicle sync daemon started", file=sys.stderr)

    requests_processed = 0
    try:
        while True:
            try:
                conn, _ = sock.accept()
            except TimeoutError:
                # Exit after idle timeout if we've processed at least one request
                if requests_processed > 0:
                    print("Idle timeout, shutting down", file=sys.stderr)
                    break
                continue

            try:
                data = b""
                while True:
                    chunk = conn.recv(4096)
                    if not chunk or b"\n" in data:
                        break
                    data += chunk

                if data:
                    request = SyncRequest.from_json(data.decode().strip())
                    _success, message = process_request(request)
                    conn.sendall(message.encode())
                    requests_processed += 1
            except (json.JSONDecodeError, KeyError) as e:
                conn.sendall(f"Invalid request: {e}\n".encode())
            finally:
                conn.close()

    except KeyboardInterrupt:
        pass
    finally:
        sock.close()

    print(f"Daemon exiting after {requests_processed} requests", file=sys.stderr)
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Radicle GitHub Sync")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # request command
    request_parser = subparsers.add_parser("request", help="Send a sync request")
    request_parser.add_argument("repo_id", help="Radicle repository ID")
    request_parser.add_argument("github_url", help="GitHub repository URL")
    request_parser.add_argument("branch", help="Branch to sync")

    # daemon command
    subparsers.add_parser("daemon", help="Run sync daemon (socket-activated)")

    args = parser.parse_args()

    match args.command:
        case "request":
            return send_request(args.repo_id, args.github_url, args.branch)
        case "daemon":
            return run_daemon()

    return 1


if __name__ == "__main__":
    sys.exit(main())
