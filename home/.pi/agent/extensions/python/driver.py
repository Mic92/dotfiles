"""Persistent Python evaluator speaking JSON lines on stdin/stdout.

Request:  {"code": str}
Response: {"stdout": str, "stderr": str, "result": str|null,
           "error": str|null, "images": [base64 png, ...]}

fd 0/1 carry the protocol and are moved to private descriptors: stray writes
from child processes or C extensions go to stderr, and anything reading stdin
(user code, spawned git/gh/ssh prompting for input) sees EOF instead of
blocking forever on - or consuming - the request stream.
"""
# ruff: noqa: INP001, S102, S307 - standalone script whose job is exec/eval

import ast
import base64
import contextlib
import io
import json
import os
import signal
import sys
import traceback
from typing import Any

os.environ.setdefault("MPLBACKEND", "Agg")

requests = os.fdopen(os.dup(0), "r")
proto = os.fdopen(os.dup(1), "w", buffering=1)
devnull = os.open(os.devnull, os.O_RDONLY)
os.dup2(devnull, 0)
os.close(devnull)
os.dup2(2, 1)
sys.stdin = os.fdopen(0, "r", closefd=False)

ns: dict[str, Any] = {"__name__": "__main__"}


def collect_figures() -> list[str]:
    plt = sys.modules.get("matplotlib.pyplot")
    if plt is None:
        return []
    images = []
    for num in plt.get_fignums():
        fig = plt.figure(num)
        buf = io.BytesIO()
        fig.savefig(buf, format="png", dpi=100, bbox_inches="tight")
        images.append(base64.b64encode(buf.getvalue()).decode())
    plt.close("all")
    return images


def run(code: str) -> dict[str, Any]:
    out, err = io.StringIO(), io.StringIO()
    result: str | None = None
    error: str | None = None
    try:
        tree = ast.parse(code, "<cell>", "exec")
        last = None
        # REPL semantics: echo the value of a trailing bare expression.
        if tree.body and isinstance(tree.body[-1], ast.Expr):
            last = ast.Expression(tree.body.pop().value)  # type: ignore[attr-defined]
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            if tree.body:
                exec(compile(tree, "<cell>", "exec"), ns)
            if last is not None:
                value = eval(compile(last, "<cell>", "eval"), ns)
                if value is not None:
                    ns["_"] = value
                    result = repr(value)
    except KeyboardInterrupt:
        error = "KeyboardInterrupt: execution interrupted"
    except BaseException as e:  # noqa: BLE001 - a REPL must survive SystemExit too
        tb = e.__traceback__
        # Hide this driver's frame so tracebacks start at <cell>.
        while tb is not None and tb.tb_frame.f_code.co_filename == __file__:
            tb = tb.tb_next
        error = "".join(traceback.format_exception(type(e), e, tb))
    return {
        "stdout": out.getvalue(),
        "stderr": err.getvalue(),
        "result": result,
        "error": error,
        "images": collect_figures(),
    }


def main() -> None:
    while True:
        # SIGINT is only meaningful while user code runs; ignore it when idle
        # so a late interrupt does not kill the session.
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        line = requests.readline()
        if not line:
            return
        req = json.loads(line)
        signal.signal(signal.SIGINT, signal.default_int_handler)
        proto.write(json.dumps(run(req["code"])) + "\n")


if __name__ == "__main__":
    main()
