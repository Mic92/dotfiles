#!/usr/bin/env python3
"""Unlock KWallet automatically using a password stored in the TPM via systemd-creds.

The KWallet password is encrypted with `systemd-creds encrypt --user` and bound
to the TPM + user identity.  At login a systemd user service decrypts it,
derives the PBKDF2-SHA512 hash that kwalletd6 expects, and sends it over D-Bus
via the pamOpen() method.

Based on https://github.com/Himalian/autokdewallet
"""

from __future__ import annotations

import hashlib
import subprocess
import sys
from pathlib import Path

import dbus  # type: ignore[import-untyped]

# PBKDF2 parameters matching KWallet's backend:
# https://github.com/KDE/kwallet/blob/master/src/backend/kwalletbackend.cpp#L100
ITERATIONS = 50000
KEY_SIZE = 56
HASH_ALGO = "sha512"

SALT_PATH = Path.home() / ".local/share/kwalletd/kdewallet.salt"


def load_salt(salt_path: Path = SALT_PATH) -> bytes:
    """Read the 56-byte binary salt that KWallet generated for this wallet."""
    if not salt_path.exists():
        msg = f"Salt file not found: {salt_path}"
        raise FileNotFoundError(msg)
    data = salt_path.read_bytes()
    if len(data) != KEY_SIZE:
        print(
            f"WARNING: expected salt length {KEY_SIZE}, got {len(data)}",
            file=sys.stderr,
        )
    return data


def decrypt_password(cred_file: Path) -> bytes:
    """Decrypt the systemd-creds encrypted credential file via TPM."""
    result = subprocess.run(
        ["systemd-creds", "decrypt", "--user", str(cred_file), "-"],
        capture_output=True,
        check=True,
    )
    return result.stdout.strip()


def derive_hash(password: bytes, salt: bytes) -> bytes:
    """Derive the PBKDF2-SHA512 hash that KWallet's PAM interface expects."""
    return hashlib.pbkdf2_hmac(HASH_ALGO, password, salt, ITERATIONS, KEY_SIZE)


def pam_open_wallet(
    password_hash: bytes,
    wallet_name: str = "kdewallet",
    timeout: int = 0,
) -> bool:
    """Send the password hash to kwalletd6 via D-Bus pamOpen()."""
    try:
        bus = dbus.SessionBus()
        proxy = bus.get_object("org.kde.kwalletd6", "/modules/kwalletd6")
        interface = dbus.Interface(proxy, "org.kde.KWallet")
        interface.pamOpen(wallet_name, dbus.ByteArray(password_hash), timeout)
        print(f"Successfully unlocked wallet: {wallet_name}")
    except dbus.DBusException as e:
        print(f"D-Bus error unlocking wallet: {e}", file=sys.stderr)
        return False
    else:
        return True


def main() -> None:
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <credential-file>", file=sys.stderr)
        sys.exit(1)

    cred_file = Path(sys.argv[1])
    if not cred_file.exists():
        print(f"Credential file not found: {cred_file}", file=sys.stderr)
        sys.exit(1)

    salt = load_salt()
    password = decrypt_password(cred_file)
    password_hash = derive_hash(password, salt)

    if not pam_open_wallet(password_hash):
        sys.exit(1)


if __name__ == "__main__":
    main()
