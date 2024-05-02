#!/usr/bin/env python3

from pathlib import Path


def bt(depth: int) -> str:
    format_args = ["%p"] * depth
    return_addrs = [f"r({i})" for i in range(depth)]
    return f"""
# -*- mode: snippet -*-
# name: poormans backtrace (depth: {depth})
# key: bt{depth}
# --

#define r(depth) __builtin_return_address(depth)
int printf(const char* f,...); printf("%s() at %s:%d\\n {" ".join(format_args)}\\n", __func__, __FILE__, __LINE__, {", ".join(return_addrs)});
#undef r
  """


def main() -> None:
    for depth in range(1, 31):
        with Path(f"c-mode/bt{depth}").open("w") as f:
            f.write(bt(depth))


if __name__ == "__main__":
    main()
