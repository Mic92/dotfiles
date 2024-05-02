#!/usr/bin/nix-shell
#!nix-shell -i bash -p fuzzel networkmanager
#shellcheck shell=bash
set -exuo pipefail

choices=("gaming" "normal")
selected=$(printf "\n%s" "${choices[@]}" | fuzzel -d)

echo You Picked: " $selected"

case $selected in
normal)
  kscreen-doctor output.DP-5.mode.3840x2160@60
  kscreen-doctor output.DP-5.scale.2
  ;;
gaming)
  kscreen-doctor output.DP-5.mode.1920x1080@60
  kscreen-doctor output.DP-5.scale.1
  ;;
*)
  echo "Invalid Option $selected"
  exit 1
  ;;
esac
