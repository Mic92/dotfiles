import os
import typing

from xonsh.built_ins import XSH
from xontrib.fzf_widgets import get_fzf_binary_path

import subprocess
from subprocess import run as r


@XSH.builtins.events.on_ptk_create
def custom_keybindings(bindings, **kw):
    @bindings.add("escape", "c")
    def fzf_cd(event):
        env = os.environ.copy()
        env["FZF_DEFAULT_COMMAND"] = "fd -t d"

        choice = r(
            [get_fzf_binary_path(), "-m", "--reverse", "--height=40%"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
            env=env,
        ).stdout.strip()
        if choice:
            XSH.aliases["cd"]([choice])


out = r(["zoxide", "init", "xonsh"], stdout=subprocess.PIPE, universal_newlines=True).stdout.strip()
XSH.builtins.execx(out, "exec", __xonsh__.ctx, filename="zoxide")


def mycd(args: typing.List[str]) -> None:
    if len(args) == 1 and os.path.isfile(args[0]):
        args[0] = os.path.dirname(args[0])
        if args[0] == "":
            args[0] = "."
    XSH.aliases["z"](args)
    XSH.builtins.execx("exa --classify --icons")


XSH.aliases["cd"] = mycd

def nixify(args: typing.List[str]) -> None:
    envrc = Path("./.envrc")
    if not envrc.exists():
        envrc.write_text("use nix\n")
        r(["direnv", "allow"])
    shell_nix = Path("shell.nix")
    default_nix = Path("default.nix")
    if not shell_nix.exists() or default_nix.exist():
        default_nix.write_text("""
with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    bashInteractive
  ];
}
        """)
        editor = os.environ.get("EDITOR", "vim")
        r([editor, default_nix])

def flakify(args: typing.List[str]) -> None:
    envrc = Path("./.envrc")
    if not envrc.exists():
        envrc.write_text("use flake\n")
        r(["direnv", "allow"])

    flake_nix = Path("flake.nix")
    if not flake_nix.exists():
        r(["nix", "flake", "new", "-t", "github:hercules-ci/flake-modules-core", "."])
    editor = os.environ.get("EDITOR", "vim")
    r([editor, flake_nix])

XSH.aliases["nixify"] = nixify
XSH.aliases["flakify"] = flakify

if XSH.env.get("WAYLAND_DISPLAY"):
    XSH.aliases["nixify"] = "chromium --enable-features=UseOzonePlatform --ozone-platform=wayland"
