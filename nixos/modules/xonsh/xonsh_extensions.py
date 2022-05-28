import os
from typing import List, IO, Optional

from xonsh.built_ins import XSH
from xontrib.fzf_widgets import get_fzf_binary_path

import subprocess
import tempfile
from pathlib import Path
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


out = r(
    ["zoxide", "init", "xonsh"], stdout=subprocess.PIPE, universal_newlines=True
).stdout.strip()
XSH.builtins.execx(out, "exec", __xonsh__.ctx, filename="zoxide")


def mycd(args: List[str]) -> None:
    if len(args) == 1 and os.path.isfile(args[0]):
        args[0] = os.path.dirname(args[0])
        if args[0] == "":
            args[0] = "."
    XSH.aliases["z"](args)
    XSH.builtins.execx("exa --classify --icons")


XSH.aliases["cd"] = mycd


def nixify(args: List[str]) -> None:
    envrc = Path("./.envrc")
    if not envrc.exists():
        envrc.write_text("use nix\n")
        r(["direnv", "allow"])
    shell_nix = Path("shell.nix")
    default_nix = Path("default.nix")
    if not shell_nix.exists() or default_nix.exist():
        default_nix.write_text(
            """
with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    bashInteractive
  ];
}
        """
        )
        editor = os.environ.get("EDITOR", "vim")
        r([editor, default_nix])


def flakify(args: List[str]) -> None:
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
    XSH.aliases[
        "chromium"
    ] = "chromium --enable-features=UseOzonePlatform --ozone-platform=wayland"


def load_package(args: List[str]) -> None:
    import json

    if len(args) == 0:
        return
    out = r(
        ["nix", "build", "--json", "-f", "<nixpkgs>"] + args, stdout=subprocess.PIPE
    )
    outputs = json.loads(out.stdout)
    for out in outputs:
        bin = out["outputs"].get("bin", out["outputs"].get("out"))
        if bin is not None:
            XSH.env["PATH"].add(f"{bin}/bin")


XSH.aliases["n"] = load_package


def kpaste(args: List[str], stdin: Optional[IO] = None) -> None:
    input = None
    if stdin:
        stdin = stdin
    elif len(args) == 0:
        input = r(["wl-paste"], stdout=subprocess.PIPE, text=True).stdout
    else:
        stdin = open(args[0])
    res = r(
        ["curl", "-sS", "http://p.r", "--data-binary", "@-"],
        stdin=stdin,
        input=input,
        stdout=subprocess.PIPE,
        text=True,
    ).stdout.strip()
    print(res)
    print(res.replace("http://p.r", "https://p.krebsco.de"))


XSH.aliases["kpaste"] = kpaste


def tempdir(args: List[str]) -> None:
    prefix = "xonsh"
    if len(args) > 0:
        prefix = args[0]
    XSH.aliases["cd"]([tempfile.mkdtemp(prefix=f"{prefix}-", dir="/tmp")])


XSH.aliases["tempdir"] = tempdir


def tmux_upterm(args: List[str]) -> None:
    r(
        [
            "upterm",
            "host",
            "--server",
            "ssh://upterm.thalheim.io:2323",
            "--force-command",
            "tmux attach -t pair-programming",
            "--",
            "bash",
            "-c",
            "read -p 'Press enter to continue ' && tmux new -t pair-programming",
        ]
    )


XSH.aliases["tmux-upterm"] = tmux_upterm


def real_which(args: List[str]) -> None:
    out = r(["which", args[0]], stdout=subprocess.PIPE, text=True).stdout.strip()
    print(Path(out).resolve())

XSH.aliases["real-which"] = real_which

def nix_call_package(args: List[str]) -> None:
    if len(args) < 1:
        args = ["./."]
    r(["nix-build", "-E", "with import <nixpkgs> {}; pkgs.callPackage " + args[0] + "{}"])
XSH.aliases["nix-call-package"] = nix_call_package
