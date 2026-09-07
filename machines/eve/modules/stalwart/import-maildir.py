"""Push dovecot Maildir++ trees into stalwart via IMAP APPEND (master login).

Preserves folders, \\Seen/\\Answered/\\Flagged/\\Deleted/\\Draft, dovecot
keywords (a-z in filename via dovecot-keywords) and internal date (mtime).
Incremental: remembers uploaded filenames per user in STATE_DIR.
"""

import argparse
import base64
import contextlib
import imaplib
import json
import os
import re
import socket
import ssl
import sys
import time
from pathlib import Path

FLAG_MAP = {
    "S": r"\Seen",
    "R": r"\Answered",
    "F": r"\Flagged",
    "T": r"\Deleted",
    "D": r"\Draft",
}
SKIP_KEYWORDS = {"$label1", "$label2", "$label3", "$label4", "$label5"}


def maildir_folders(root: Path) -> dict[str, Path]:
    out = {"INBOX": root}
    for d in root.iterdir():
        if d.name.startswith(".") and (d / "cur").is_dir():
            out[d.name[1:].replace(".", "/")] = d
    return out


def read_keywords(folder: Path) -> dict[str, str]:
    f = folder / "dovecot-keywords"
    if not f.exists():
        return {}
    kw = {}
    for line in f.read_text().splitlines():
        idx, _, name = line.partition(" ")
        if idx.isdigit() and int(idx) < 26:
            kw[chr(ord("a") + int(idx))] = name
    return kw


def parse_flags(name: str, keywords: dict[str, str]) -> list[str]:
    m = re.search(r":2,([A-Za-z]*)$", name)
    if not m:
        return []
    flags = []
    for c in m.group(1):
        if c in FLAG_MAP:
            flags.append(FLAG_MAP[c])
        elif c in keywords and keywords[c] not in SKIP_KEYWORDS:
            k = keywords[c]
            if re.fullmatch(r"[A-Za-z0-9$_.\-]+", k):
                flags.append(k)
    return flags


def base_name(name: str) -> str:
    return name.split(":2,", maxsplit=1)[0]


def connect(host: str, port: int, login: str, pw: str) -> imaplib.IMAP4:
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    imap = imaplib.IMAP4_SSL(host, port, ssl_context=ctx)
    imap.login(login, pw)
    return imap


def import_user(args: argparse.Namespace, user: str, maildir: Path, pw: str) -> int:
    state_file = Path(args.state_dir) / f"{user}.json"
    done: dict[str, list[str]] = (
        json.loads(state_file.read_text()) if state_file.exists() else {}
    )
    imap = connect(args.host, args.port, f"{user}%admin", pw)
    existing = set()
    typ, data = imap.list()
    for line in data or []:
        if line:
            m = re.search(rb'"([^"]*)"$|([^ ]+)$', line)
            existing.add((m.group(1) or m.group(2)).decode())
    errors = 0
    uploaded = 0
    for name, path in sorted(maildir_folders(maildir).items()):
        if name.split("/")[0] == "Virtual":
            continue
        if name not in existing and name != "INBOX":
            typ, resp = imap.create('"' + name.replace('"', '\\"') + '"')
            if typ != "OK" and b"ALREADYEXISTS" not in (resp[0] or b""):
                print(f"  ! create {name}: {resp}", file=sys.stderr)
                errors += 1
                continue
            existing.add(name)
        keywords = read_keywords(path)
        seen = set(done.get(name, []))
        files = [
            p
            for sub in ("cur", "new")
            if (path / sub).is_dir()
            for p in (path / sub).iterdir()
            if p.is_file()
        ]
        todo = [p for p in files if base_name(p.name) not in seen]
        if not todo:
            continue
        print(f"  {name}: {len(todo)} new of {len(files)}")
        mbox = '"' + name.replace('"', '\\"') + '"'
        n = 0
        for p in todo:
            flags = parse_flags(p.name, keywords)
            try:
                msg = p.read_bytes()
            except OSError as e:
                print(f"  ! read {p}: {e}", file=sys.stderr)
                errors += 1
                continue
            date = imaplib.Time2Internaldate(p.stat().st_mtime)
            for _attempt in range(3):
                try:
                    typ, resp = imap.append(
                        mbox, "(" + " ".join(flags) + ")", date, msg
                    )
                    break
                except (imaplib.IMAP4.abort, OSError):
                    time.sleep(2)
                    imap = connect(args.host, args.port, f"{user}%admin", pw)
                except imaplib.IMAP4.error as e:
                    typ, resp = "NO", [str(e).encode()]
                    break
            else:
                typ, resp = "NO", [b"gave up"]
            if typ != "OK":
                print(f"  ! append {p.name} -> {name}: {resp}", file=sys.stderr)
                errors += 1
                continue
            seen.add(base_name(p.name))
            n += 1
            uploaded += 1
            if n % 500 == 0:
                done[name] = sorted(seen)
                state_file.write_text(json.dumps(done))
        done[name] = sorted(seen)
        state_file.write_text(json.dumps(done))
    with contextlib.suppress(imaplib.IMAP4.error, OSError):
        imap.logout()
    print(f"  uploaded {uploaded}, errors {errors}")
    return errors


def upload_sieve(args: argparse.Namespace, user: str, home: Path, pw: str) -> None:
    active = home / "sieve" / "active-script.sieve"
    if not active.exists():
        return
    script = active.resolve().read_bytes()
    raw = socket.create_connection((args.host, args.sieve_port))
    f = raw.makefile("rwb")

    def resp() -> None:
        while True:
            line = f.readline().decode()
            if not line:
                raise RuntimeError("managesieve: connection closed")
            if line.startswith(("OK", "NO", "BYE")):
                if not line.startswith("OK"):
                    raise RuntimeError(f"managesieve: {line.strip()}")
                return

    def cmd(s: bytes) -> None:
        f.write(s + b"\r\n")
        f.flush()
        resp()

    resp()
    f.write(b"STARTTLS\r\n")
    f.flush()
    resp()
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    tls = ctx.wrap_socket(raw)
    f = tls.makefile("rwb")
    resp()
    auth = base64.b64encode(b"\0" + f"{user}%admin".encode() + b"\0" + pw.encode())
    cmd(b'AUTHENTICATE "PLAIN" "' + auth + b'"')
    cmd(b'PUTSCRIPT "dovecot" {' + str(len(script)).encode() + b"+}\r\n" + script)
    cmd(b'SETACTIVE "dovecot"')
    print(f"  sieve: uploaded {active}")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--port", type=int, default=2993)
    ap.add_argument("--sieve-port", type=int, default=4191)
    ap.add_argument("--vmail", default="/var/vmail")
    ap.add_argument("--state-dir", default="/var/lib/stalwart/import-state")
    ap.add_argument(
        "--password-file", default=os.environ.get("STALWART_ADMIN_PASSWORD_FILE")
    )
    ap.add_argument("users", nargs="*", help="user@domain, default: all under --vmail")
    args = ap.parse_args()

    pw = Path(args.password_file).read_text().strip()
    Path(args.state_dir).mkdir(parents=True, exist_ok=True)
    vmail = Path(args.vmail)
    users = args.users or sorted(
        f"{u.name}@{d.name}"
        for d in vmail.iterdir()
        if d.is_dir()
        for u in d.iterdir()
        if (u / "Maildir").is_dir()
    )
    failed = []
    for user in users:
        local, domain = user.split("@")
        home = vmail / domain / local
        print(f">>> {user}")
        try:
            errs = import_user(args, user, home / "Maildir", pw)
            upload_sieve(args, user, home, pw)
        except (imaplib.IMAP4.error, OSError, RuntimeError) as e:
            print(f"  !! {e}", file=sys.stderr)
            errs = 1
        if errs:
            failed.append(user)
    if failed:
        print("failed:", " ".join(failed), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
