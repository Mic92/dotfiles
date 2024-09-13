#!/usr/bin/env bash

set -eux -o pipefail
shopt -s nullglob

# Choose the most recent file.
best=
bestdate=0
for cand in ~/.openttd/save/*.sav ~/.openttd/save/autosave/*.sav; do
  # Choose this candidate if it is newer than the best so far.
  canddate="$(date +'%s%N' -r "$cand")"
  if [[ $canddate -gt $bestdate ]]; then
    best="$cand"
    bestdate="$canddate"
  fi
done

password_file=$HOME/.openttd/password
if [[ ! -f $password_file ]]; then
  mkdir -p "$(dirname "$password_file")"
  (
    trap '' pipe # head will cause SIGPIPE in tr process
    tr -dc _A-Z-a-z-0-9 </dev/urandom | head -c10 >"$password_file"
  )
fi
password=$(cat "$password_file")
echo "Server password: ${password}"

load_arg=

if [[ -n $best ]]; then
  load_arg="-g $best"
fi

# Run a dedicated server with the best file.
exec openttd "$load_arg" -p "$password" -D "$@"
