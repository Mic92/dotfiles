import argparse
import platform
import sys

import keyring

from .pinentry import Pinentry


def build_parser() -> argparse.ArgumentParser:
    backend_info = {
        "Darwin": "macOS Keychain",
        "Linux": "Secret Service (KDE Wallet, GNOME Keyring)",
    }
    current_backend = backend_info.get(platform.system(), "system secure storage")
    description = (
        "rbw pinentry wrapper using system secure storage. "
        f"Current platform: {platform.system()} using {current_backend}."
    )
    epilog = (
        "Setup:\n"
        "  Configure rbw: rbw config set pinentry rbw-pinentry\n\n"
        "The password is stored securely in your system's secure storage:\n"
        "- macOS: Login Keychain\n"
        "- Linux: Secret Service API (KDE Wallet, GNOME Keyring, etc.)\n\n"
        "How it works:\n"
        "- First time: Prompts for password and caches it in secure storage\n"
        "- Subsequent uses: Returns cached password from secure storage\n"
        "- If cached password fails: Automatically clears it and re-prompts"
    )
    parser = argparse.ArgumentParser(
        prog="rbw-pinentry",
        description=description,
        epilog=epilog,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "-c",
        "--clear",
        action="store_true",
        help="Clear the stored master password from secure storage and exit",
    )
    # Arguments passed by rbw-agent (ignored for compatibility)
    parser.add_argument("--timeout", type=str, help=argparse.SUPPRESS)
    parser.add_argument("--ttyname", type=str, help=argparse.SUPPRESS)
    parser.add_argument("--display", type=str, help=argparse.SUPPRESS)
    parser.add_argument("--no-global-grab", action="store_true", help=argparse.SUPPRESS)
    return parser


def main() -> None:
    parser = build_parser()
    args = parser.parse_args(sys.argv[1:])

    if args.clear:
        pinentry = Pinentry()
        try:
            keyring.delete_password(pinentry.service_name, pinentry.rbw_profile)
            print(f"Cleared password for profile: {pinentry.rbw_profile}")
        except keyring.errors.PasswordDeleteError:
            print(f"No password found for profile: {pinentry.rbw_profile}")
        except keyring.errors.KeyringError as e:
            print(f"Failed to clear password: {e}", file=sys.stderr)
            sys.exit(1)
        sys.exit(0)

    pinentry = Pinentry()
    pinentry.handle_pinentry_session()
