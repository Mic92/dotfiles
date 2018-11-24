#!/bin/sh
# Copyright (c) 2012 The Chromium OS Authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.
# Max length of the OSC 52 sequence.  Sequences longer than this will not be
# sent to the terminal.
OSC_52_MAX_SEQUENCE="100000"
# Write an error message and exit.
# Usage: <message>
die() {
  echo "ERROR: $*"
  exit 1
}
# Send a DCS sequence through tmux.
# Usage: <sequence>
tmux_dcs() {
  printf '\033Ptmux;\033%s\033\\' "$1"
}
# Send a DCS sequence through screen.
# Usage: <sequence>
screen_dcs() {
  # Screen limits the length of string sequences, so we have to break it up.
  # Going by the screen history:
  #   (v4.2.1) Apr 2014 - today: 768 bytes
  #   Aug 2008 - Apr 2014 (v4.2.0): 512 bytes
  #   ??? - Aug 2008 (v4.0.3): 256 bytes
  # Since v4.2.0 is only ~4 years old, we'll use the 256 limit.
  # We can probably switch to the 768 limit in 2022.
  local limit=256
  # We go 4 bytes under the limit because we're going to insert two bytes
  # before (\eP) and 2 bytes after (\e\) each string.
  echo "$1" | \
    sed -E "s:.{$(( limit - 4 ))}:&\n:g" | \
    sed -E -e 's:^:\x1bP:' -e 's:$:\x1b\\:' | \
    tr -d '\n'
}
# Send an escape sequence to hterm.
# Usage: <sequence>
print_seq() {
  local seq="$1"
  case ${TERM-} in
  screen*)
    # Since tmux defaults to setting TERM=screen (ugh), we need to detect
    # it here specially.
    if [ -n "${TMUX-}" ]; then
      tmux_dcs "${seq}"
    else
      screen_dcs "${seq}"
    fi
    ;;
  tmux*)
    tmux_dcs "${seq}"
    ;;
  *)
    echo "${seq}"
    ;;
  esac
}
# Base64 encode stdin.
b64enc() {
  base64 | tr -d '\n'
}
# Send the OSC 52 sequence to copy the content.
# Usage: [string]
copy() {
  local str
  if [ $# -eq 0 ]; then
    str="$(b64enc)"
  else
    str="$(echo "$@" | b64enc)"
  fi
  local len=${#str}
  if [ ${len} -lt ${OSC_52_MAX_SEQUENCE} ]; then
    print_seq "$(printf '\033]52;c;%s\a' "${str}")"
  else
    die "selection too long to send to terminal:" \
      "${OSC_52_MAX_SEQUENCE} limit, ${len} attempted"
  fi
}
# Write tool usage and exit.
# Usage: [error message]
usage() {
  if [ $# -gt 0 ]; then
    exec 1>&2
  fi
  cat <<EOF
Usage: osc52 [options] [string]
Send an arbitrary string to the terminal clipboard using the OSC 52 escape
sequence as specified in xterm:
  https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
  Section "Operating System Controls", Ps => 52.
The data can either be read from stdin:
  $ echo "hello world" | osc52.sh
Or specified on the command line:
  $ osc52.sh "hello world"
EOF
  if [ $# -gt 0 ]; then
    echo
    die "$@"
  else
    exit 0
  fi
}
main() {
  set -e
  while [ $# -gt 0 ]; do
    case $1 in
    -h|--help)
      usage
      ;;
    -*)
      usage "Unknown option: $1"
      ;;
    *)
      break
      ;;
    esac
  done
  copy "$@"
}
main "$@"
