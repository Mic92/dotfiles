#!/usr/bin/env python3
"""
systemctl compatibility layer for macOS using launchd
Maps systemctl commands to launchctl equivalents
"""

import argparse
import contextlib
import os
import plistlib
import shutil
import subprocess
import sys
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path


class LaunchctlService:
    """Represents a launchd service"""

    def __init__(self, name: str, plist_path: Path | None = None) -> None:
        self.name = name
        self.plist_path = plist_path
        self.is_user_service = bool(plist_path and "LaunchAgents" in str(plist_path))

    @property
    def domain_target(self) -> str:
        """Get the launchctl domain target"""
        if self.is_user_service:
            return f"gui/{os.getuid()}/{self.name}"
        return f"system/{self.name}"


class SystemctlMacOS:
    """systemctl implementation for macOS using launchd"""

    def __init__(self) -> None:
        self.user_agents_dir = Path.home() / "Library" / "LaunchAgents"
        self.system_daemons_dir = Path("/Library/LaunchDaemons")
        self.system_agents_dir = Path("/System/Library/LaunchAgents")
        self.system_launch_daemons_dir = Path("/System/Library/LaunchDaemons")

    def find_service(self, service_name: str) -> LaunchctlService | None:
        """Find a service plist file"""
        search_paths = [
            self.user_agents_dir,
            self.system_daemons_dir,
            self.system_agents_dir,
            self.system_launch_daemons_dir,
        ]

        for search_dir in search_paths:
            if not search_dir.exists():
                continue

            # Try exact match first
            plist_file = search_dir / f"{service_name}.plist"
            if plist_file.exists():
                return LaunchctlService(service_name, plist_file)

            # Try glob match for partial names
            matches = list(search_dir.glob(f"*{service_name}*.plist"))
            if matches:
                plist_path = Path(matches[0])
                # Extract service name from filename
                name = plist_path.stem
                return LaunchctlService(name, plist_path)

        return None

    def run_launchctl(self, args: list[str]) -> subprocess.CompletedProcess[str]:
        """Run launchctl command"""
        cmd = ["launchctl", *args]
        try:
            return subprocess.run(cmd, capture_output=True, text=True, check=True)
        except subprocess.CalledProcessError as e:
            print(f"launchctl error: {e.stderr}", file=sys.stderr)
            sys.exit(e.returncode)

    def start(self, service_name: str) -> None:
        """Start a service"""
        service = self.find_service(service_name)
        if not service:
            print(f"Failed to start {service_name}: Unit not found.", file=sys.stderr)
            sys.exit(1)

        if service.plist_path:
            self.run_launchctl(["load", str(service.plist_path)])
        else:
            self.run_launchctl(["kickstart", service.domain_target])

        print(f"Started {service_name}")

    def stop(self, service_name: str) -> None:
        """Stop a service"""
        service = self.find_service(service_name)
        if not service:
            print(f"Failed to stop {service_name}: Unit not found.", file=sys.stderr)
            sys.exit(1)

        if service.plist_path:
            self.run_launchctl(["unload", str(service.plist_path)])
        else:
            self.run_launchctl(["kill", "TERM", service.domain_target])

        print(f"Stopped {service_name}")

    def restart(self, service_name: str) -> None:
        """Restart a service"""
        with contextlib.suppress(SystemExit):
            self.stop(service_name)
        self.start(service_name)

    def get_service_logs(self, service: LaunchctlService, lines: int = 10) -> list[str]:
        """Get recent logs for a service from StandardOutPath/StandardErrorPath"""
        if not (service.plist_path and service.plist_path.exists()):
            return []

        log_files = []

        # Extract log file paths from plist
        try:
            with service.plist_path.open("rb") as f:
                plist = plistlib.load(f)

                if "StandardOutPath" in plist:
                    log_files.append(Path(plist["StandardOutPath"]))

                if "StandardErrorPath" in plist:
                    log_files.append(Path(plist["StandardErrorPath"]))

        except (OSError, plistlib.InvalidFileException):
            return []

        # Read log files and combine output
        all_log_lines: list[str] = []
        for log_file in log_files:
            if log_file.exists():
                try:
                    with log_file.open("r") as f:
                        file_lines = f.readlines()
                        # Add file indicator for stderr vs stdout
                        file_type = (
                            "stderr" if "error" in log_file.name.lower() else "stdout"
                        )
                        all_log_lines.extend(
                            f"[{file_type}] {line.rstrip()}" for line in file_lines
                        )
                except (OSError, UnicodeDecodeError):
                    continue

        # Return last N lines
        return all_log_lines[-lines:] if len(all_log_lines) > lines else all_log_lines

    def find_service_in_list(
        self, service: LaunchctlService, lines_output: list[str]
    ) -> tuple[bool, str, str, str]:
        """Find service in launchctl list output."""
        for line in lines_output[1:]:  # Skip header
            if not line.strip():
                continue
            parts = line.split("\t")
            if len(parts) >= 3 and service.name in parts[2]:
                pid = parts[0] if parts[0] != "-" else "inactive"
                exit_code = parts[1] if parts[1] != "-" else "0"
                label = parts[2]
                return True, pid, exit_code, label
        return False, "", "", ""

    def print_service_status(
        self, service_name: str, found: bool, pid: str, exit_code: str, label: str
    ) -> None:
        """Print service status information."""
        if found:
            status_text = "active (running)" if pid != "inactive" else "inactive (dead)"
            print(f"● {service_name} - {label}")
            print("   Loaded: loaded")
            print(f"   Active: {status_text}")
            if pid != "inactive":
                print(f"   Main PID: {pid}")
            if exit_code != "0":
                print(f"   Exit Code: {exit_code}")
        else:
            print(f"● {service_name}")
            print(f"   Loaded: not-found (Reason: Unit {service_name} not found.)")
            print("   Active: inactive (dead)")

    def print_service_logs(
        self, service: LaunchctlService | None, found: bool, log_lines: int
    ) -> None:
        """Print service logs."""
        print()
        if found and service:
            log_entries = self.get_service_logs(service, log_lines)
            if log_entries:
                print("Recent Logs:")
                for entry in log_entries:
                    if entry.strip():
                        print(f"   {entry}")
            else:
                print("Recent Logs:")
                print(
                    "   No log files found (service needs StandardOutPath/StandardErrorPath in plist)"
                )
        else:
            print("Recent Logs:")
            print("   No logs found (service not found)")

    def status(self, service_name: str, log_lines: int = 10) -> None:
        """Show service status"""
        service = self.find_service(service_name)
        if not service:
            print(f"Unit {service_name} could not be found.")
            sys.exit(4)

        result = self.run_launchctl(["list"])
        lines_output = result.stdout.split("\n")

        # Look for the service in the output
        found, pid, exit_code, label = self.find_service_in_list(service, lines_output)

        # Print status
        self.print_service_status(service_name, found, pid, exit_code, label)

        # Print logs
        self.print_service_logs(service, found, log_lines)

    def enable(self, service_name: str) -> None:
        """Enable a service"""
        service = self.find_service(service_name)
        if not service or not service.plist_path:
            print(f"Failed to enable {service_name}: Unit not found.", file=sys.stderr)
            sys.exit(1)

        # For user services, copy to user LaunchAgents
        if not service.is_user_service:
            target = self.user_agents_dir / f"{service_name}.plist"
            self.user_agents_dir.mkdir(parents=True, exist_ok=True)

            # Create symlink or copy
            if not target.exists():
                try:
                    target.symlink_to(service.plist_path)
                except OSError:
                    # Fallback to copy if symlink fails
                    shutil.copy2(service.plist_path, target)

        print(f"Enabled {service_name}")

    def disable(self, service_name: str) -> None:
        """Disable a service"""
        # Remove from user LaunchAgents
        user_plist = self.user_agents_dir / f"{service_name}.plist"
        if user_plist.exists():
            user_plist.unlink()

        print(f"Disabled {service_name}")

    def cat(self, service_name: str) -> None:
        """Show service plist contents"""
        service = self.find_service(service_name)
        if not service or not service.plist_path:
            print(f"No files found for {service_name}", file=sys.stderr)
            sys.exit(1)

        # Try to use bat for syntax highlighting if available
        if shutil.which("bat"):
            try:
                subprocess.run(
                    [
                        "bat",
                        "--style=header,grid",
                        "--language=xml",
                        str(service.plist_path),
                    ],
                    check=False,
                )
            except subprocess.CalledProcessError:
                pass
            else:
                return

        # Fallback to regular cat
        print(f"# {service.plist_path}")
        try:
            with service.plist_path.open() as f:
                print(f.read())
        except OSError as e:
            print(f"Failed to read {service.plist_path}: {e}", file=sys.stderr)
            sys.exit(1)

    def list_units(self, service_filter: str = "") -> None:
        """List all units, optionally filtered by service name"""
        result = self.run_launchctl(["list"])
        lines = result.stdout.split("\n")

        print(
            "UNIT".ljust(50),
            "LOAD".ljust(10),
            "ACTIVE".ljust(10),
            "SUB".ljust(10),
            "DESCRIPTION",
        )
        print("-" * 80)

        for line in lines[1:]:  # Skip header
            if not line.strip():
                continue
            parts = line.split("\t")
            if len(parts) >= 3:
                pid = parts[0] if parts[0] != "-" else ""
                label = parts[2]

                # Filter by service name if provided
                if service_filter and service_filter not in label:
                    continue

                unit_name = label
                load_state = "loaded"
                active_state = "active" if pid else "inactive"
                sub_state = "running" if pid else "dead"

                print(
                    unit_name.ljust(50),
                    load_state.ljust(10),
                    active_state.ljust(10),
                    sub_state.ljust(10),
                    label,
                )


@dataclass
class Args:
    """Command line arguments"""

    command: Callable[[str], None]
    service: str
    unit_type: str | None = None
    user: bool = False
    lines: int = 10


def parse_args() -> Args:  # noqa: PLR0915
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(description="systemctl compatibility for macOS")
    subparsers = parser.add_subparsers(dest="command", required=True)

    systemctl = SystemctlMacOS()

    # Start command
    start_parser = subparsers.add_parser("start", help="Start a service")
    start_parser.add_argument("service", help="Service name")
    start_parser.add_argument("--user", action="store_true", help="Run in user mode")
    start_parser.set_defaults(func=systemctl.start)

    # Stop command
    stop_parser = subparsers.add_parser("stop", help="Stop a service")
    stop_parser.add_argument("service", help="Service name")
    stop_parser.add_argument("--user", action="store_true", help="Run in user mode")
    stop_parser.set_defaults(func=systemctl.stop)

    # Restart command
    restart_parser = subparsers.add_parser("restart", help="Restart a service")
    restart_parser.add_argument("service", help="Service name")
    restart_parser.add_argument("--user", action="store_true", help="Run in user mode")
    restart_parser.set_defaults(func=systemctl.restart)

    # Status command
    status_parser = subparsers.add_parser("status", help="Show service status")
    status_parser.add_argument("service", help="Service name")
    status_parser.add_argument("--user", action="store_true", help="Run in user mode")
    status_parser.add_argument(
        "-n",
        "--lines",
        type=int,
        default=10,
        help="Number of recent log lines to show (default: 10)",
    )
    status_parser.set_defaults(func=systemctl.status)

    # Enable command
    enable_parser = subparsers.add_parser("enable", help="Enable a service")
    enable_parser.add_argument("service", help="Service name")
    enable_parser.add_argument("--user", action="store_true", help="Run in user mode")
    enable_parser.set_defaults(func=systemctl.enable)

    # Disable command
    disable_parser = subparsers.add_parser("disable", help="Disable a service")
    disable_parser.add_argument("service", help="Service name")
    disable_parser.add_argument("--user", action="store_true", help="Run in user mode")
    disable_parser.set_defaults(func=systemctl.disable)

    # Cat command
    cat_parser = subparsers.add_parser("cat", help="Show service plist contents")
    cat_parser.add_argument("service", help="Service name")
    cat_parser.add_argument("--user", action="store_true", help="Run in user mode")
    cat_parser.set_defaults(func=systemctl.cat)

    # List-units command
    list_parser = subparsers.add_parser("list-units", help="List all units")
    list_parser.add_argument(
        "service", nargs="?", default="", help="Service name filter"
    )
    list_parser.add_argument("--type", help="Unit type filter")
    list_parser.add_argument("--user", action="store_true", help="Run in user mode")
    list_parser.set_defaults(func=systemctl.list_units)

    # Is-active command
    active_parser = subparsers.add_parser(
        "is-active", help="Check if service is active"
    )
    active_parser.add_argument("service", help="Service name")
    active_parser.add_argument("--user", action="store_true", help="Run in user mode")

    def is_active_wrapper(service: str) -> None:
        svc = systemctl.find_service(service)
        if svc:
            result = systemctl.run_launchctl(["list"])
            if svc.name in result.stdout:
                print("active")
                sys.exit(0)
        print("inactive")
        sys.exit(3)

    active_parser.set_defaults(func=is_active_wrapper)

    # Is-enabled command
    enabled_parser = subparsers.add_parser(
        "is-enabled", help="Check if service is enabled"
    )
    enabled_parser.add_argument("service", help="Service name")
    enabled_parser.add_argument("--user", action="store_true", help="Run in user mode")

    def is_enabled_wrapper(service: str) -> None:
        user_plist = systemctl.user_agents_dir / f"{service}.plist"
        if user_plist.exists():
            print("enabled")
            sys.exit(0)
        print("disabled")
        sys.exit(1)

    enabled_parser.set_defaults(func=is_enabled_wrapper)

    parsed_args = parser.parse_args()
    return Args(
        command=parsed_args.func,
        service=getattr(parsed_args, "service", ""),
        unit_type=getattr(parsed_args, "type", None),
        user=getattr(parsed_args, "user", False),
        lines=getattr(parsed_args, "lines", 10),
    )


def main() -> None:
    args = parse_args()

    try:
        # Handle status command with lines parameter
        if args.command.__name__ == "status":
            # mypy: ignore since we know status takes two args
            args.command(args.service, args.lines)  # type: ignore[call-arg]
        else:
            args.command(args.service)

    except KeyboardInterrupt:
        sys.exit(130)


if __name__ == "__main__":
    main()
