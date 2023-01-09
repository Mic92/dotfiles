#! /usr/bin/env bash
set -euf -x

green() { printf '\x0303,99%s\x0F' "$1"; }
red() { printf '\x0304,99%s\x0F' "$1"; }
orange() { printf '\x0307,99%s\x0F' "$1"; }
pink() { printf '\x0313,99%s\x0F' "$1"; }
gray() { printf '\x0314,99%s\x0F' "$1"; }

unset message
add_message() {
  message="${message+$message
}$*"
}

empty=0000000000000000000000000000000000000000

while read -r oldrev newrev ref; do

  if [ "$oldrev" = "$empty" ]; then
    receive_mode=create
  elif [ "$newrev" = "$empty" ]; then
    receive_mode=delete
  elif [ "$(git merge-base "$oldrev" "$newrev")" = "$oldrev" ]; then
    receive_mode=fast-forward
  else
    receive_mode=non-fast-forward
  fi

  # shellcheck disable=SC2001
  h=$(echo "$ref" | sed 's:^refs/heads/::')

  empty_tree=4b825dc6

  id=$(echo "$newrev" | cut -b-7)
  id2=$(echo "$oldrev" | cut -b-7)
  if [ "$newrev" = "$empty" ]; then id=$empty_tree; fi
  if [ "$oldrev" = "$empty" ]; then id2=$empty_tree; fi

  link="${GIT_URL-} $h"

  add_message "$(pink push) $link $(gray "($receive_mode)")"

  add_message "$(
    git log \
      --format="$(orange %h) %s $(gray '(%ar)')" \
      --reverse \
      "$id2..$id"

    git diff --stat "$id2..$id" |
      sed '$!s/\(+*\)\(-*\)$/'"$(green '\1')$(red '\2')"'/'
  )"
done

if test -n "${message-}"; then
  echo "$message" | ircsink "$@"
fi
