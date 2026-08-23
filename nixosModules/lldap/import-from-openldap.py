#!/usr/bin/env python3
"""Convert an OpenLDAP LDIF export to SQL for the lldap database.

Reads LDIF (users under ou=users,dc=eve and groups under ou=groups,dc=eve)
on stdin and prints SQL on stdout. Designed as a pipeline so password
hashes are never written to disk:

    ldapsearch -x -D cn=admin,dc=eve -y <pw> -b dc=eve -LLL \\
        '(|(objectClass=person)(objectClass=groupOfNames))' '*' \\
      | ./import-from-openldap.py | psql -d lldap -v ON_ERROR_STOP=1

Users are keyed by the localpart of their mail address. Password hashes
in {SSHA}/{ARGON2}/{CRYPT} format are imported verbatim; the patched
lldap verifies them on LDAP binds and upgrades them to OPAQUE records.
Plaintext passwords (OpenLDAP's no-prefix convention) are converted to
{SSHA} first. Accounts under ou=system are skipped: service accounts
get fresh credentials during the migration.

Members of cn=admins are also added to lldap's built-in lldap_admin
group. The statements use ON CONFLICT DO NOTHING, so the import is
idempotent and never overwrites existing lldap users.
"""

import base64
import hashlib
import os
import sys
from dataclasses import dataclass

LEGACY_PREFIXES = (b"{SSHA}", b"{ARGON2}", b"{CRYPT}")


def parse_ldif(text: str) -> list[dict[str, list[bytes]]]:
    entries: list[dict[str, list[bytes]]] = []
    entry: dict[str, list[bytes]] = {}
    # Unfold continuation lines (RFC 2849).
    lines = text.replace("\n ", "").splitlines()
    for line in lines:
        if not line.strip():
            if entry:
                entries.append(entry)
                entry = {}
            continue
        if line.startswith("#"):
            continue
        key, _, value = line.partition(":")
        if value.startswith(":"):
            decoded = base64.b64decode(value[1:].strip())
        else:
            decoded = value.strip().encode()
        entry.setdefault(key.lower(), []).append(decoded)
    if entry:
        entries.append(entry)
    return entries


def sql_str(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def sql_bytea(value: bytes) -> str:
    return f"decode('{base64.b64encode(value).decode()}', 'base64')"


def ssha(password: bytes) -> bytes:
    salt = os.urandom(8)
    # SSHA is defined as salted SHA-1; the patched lldap upgrades it to
    # an OPAQUE record on first bind anyway.
    digest = hashlib.sha1(password + salt).digest()  # noqa: S324
    return b"{SSHA}" + base64.b64encode(digest + salt)


def normalize_password(raw: bytes) -> bytes:
    if raw.startswith(LEGACY_PREFIXES):
        return raw
    # No known scheme prefix: OpenLDAP stores these as plaintext.
    return ssha(raw)


@dataclass
class User:
    uid: str
    mail: str
    display_name: str
    password: bytes | None


def collect(
    entries: list[dict[str, list[bytes]]],
) -> tuple[dict[str, User], list[tuple[str, list[str]]]]:
    users: dict[str, User] = {}  # dn -> user
    groups: list[tuple[str, list[str]]] = []

    for entry in entries:
        dn = entry.get("dn", [b""])[0].decode()
        classes = {
            c.lower() for c in b"|".join(entry.get("objectclass", [])).split(b"|")
        }
        if b"groupofnames" in classes and dn.endswith("ou=groups,dc=eve"):
            name = entry["cn"][0].decode().strip()
            members = [m.decode() for m in entry.get("member", [])]
            groups.append((name, members))
        elif dn.endswith("ou=users,dc=eve") and dn != "ou=users,dc=eve":
            if "ou=system," in dn:
                continue
            mail = entry.get("mail", [b""])[0].decode().strip().lower()
            if "@" not in mail:
                print(f"-- skipped (no mail): {dn}")
                continue
            uid = mail.split("@")[0]
            if any(u.uid == uid for u in users.values()):
                msg = f"duplicate user id {uid!r} for {dn}"
                raise SystemExit(msg)
            password = entry.get("userpassword", [None])[0]
            users[dn] = User(
                uid=uid,
                mail=mail,
                display_name=entry.get("cn", [uid.encode()])[0].strip().decode(),
                password=normalize_password(password) if password else None,
            )
    return users, groups


def emit_users(users: dict[str, User]) -> None:
    for user in users.values():
        pw_sql = sql_bytea(user.password) if user.password is not None else "NULL"
        print(
            "INSERT INTO users (user_id, email, lowercase_email, creation_date,"  # noqa: S608
            " uuid, display_name, password_hash) VALUES"
            f" ({sql_str(user.uid)}, {sql_str(user.mail)}, {sql_str(user.mail)}, now(),"
            f" gen_random_uuid()::text, {sql_str(user.display_name)}, {pw_sql})"
            " ON CONFLICT (user_id) DO NOTHING;"
        )


def emit_groups(users: dict[str, User], groups: list[tuple[str, list[str]]]) -> None:
    for name, members in groups:
        print(
            "INSERT INTO groups (display_name, lowercase_display_name,"  # noqa: S608
            f" creation_date, uuid) VALUES ({sql_str(name)}, {sql_str(name.lower())},"
            " now(), gen_random_uuid()::text)"
            " ON CONFLICT (display_name) DO NOTHING;"
        )
        target_groups = [name, "lldap_admin"] if name == "admins" else [name]
        for member_dn in members:
            member = users.get(member_dn)
            if member is None:
                print(f"-- skipped member (not imported): {member_dn}")
                continue
            for group in target_groups:
                print(
                    "INSERT INTO memberships (user_id, group_id)"  # noqa: S608
                    f" SELECT {sql_str(member.uid)}, group_id FROM groups"
                    f" WHERE display_name = {sql_str(group)}"
                    " ON CONFLICT (user_id, group_id) DO NOTHING;"
                )


def main() -> None:
    users, groups = collect(parse_ldif(sys.stdin.read()))
    print("BEGIN;")
    emit_users(users)
    emit_groups(users, groups)
    print("COMMIT;")


if __name__ == "__main__":
    main()
