import subprocess
import os
import typing

from xonsh.built_ins import XSH
from xontrib.fzf_widgets import get_fzf_binary_path


@XSH.builtins.events.on_ptk_create
def custom_keybindings(bindings, **kw):
    # from import xontrib.fzf_widgets import fzf_insert_file
    @bindings.add("escape", "c")
    def fzf_cd(event):
        env = os.environ.copy()
        env["FZF_DEFAULT_COMMAND"] = "fd -t d"

        choice = subprocess.run(
            [get_fzf_binary_path(), "-m", "--reverse", "--height=40%"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
            env=env,
        ).stdout.strip()
        if choice:
            XSH.aliases["cd"]([choice])


out = subprocess.run(
    ["zoxide", "init", "xonsh"], stdout=subprocess.PIPE, universal_newlines=True
).stdout.strip()
XSH.builtins.execx(out, "exec", __xonsh__.ctx, filename="zoxide")


def mycd(args: typing.List[str]) -> None:
    XSH.builtins.execx("exa --classify --icons")
    if len(args) == 1 and os.path.isfile(args[0]):
        args[0] = os.path.dirname(args[0])
        if args[0] == "":
            args[0] = "."
    XSH.aliases["z"](args)


XSH.aliases["cd"] = mycd
