#!/usr/bin/env python3


def bt(depth) -> str:
    format_args = []
    for i in range(depth):
        format_args.append("%p")
    return_addrs = []
    for i in range(depth):
        return_addrs.append(f"r({i})")
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
        with open(f"c-mode/bt{depth}", "w") as f:
            f.write(bt(depth))


if __name__ == "__main__":
    main()
