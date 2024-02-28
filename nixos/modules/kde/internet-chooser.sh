#!/usr/bin/nix-shell
#!nix-shell -i bash -p fuzzel networkmanager
#shellcheck shell=bash
set -exuo pipefail

choices=("bluetooth" "phone-wifi" "default")
selected=$(printf "\n%s" "${choices[@]}" | fuzzel -d)

echo You Picked: " $selected"

case $selected in
bluetooth)
  nmcli c up "Pixel 8 Network"
  ;;
phone-wifi)
  nmcli c up the.network
  ;;
default)
  nmcli c down the.network || true
  nmcli c down "Pixel 8 Network" || true
  ;;
*)
  echo "Invalid Option $selected"
  exit 1
  ;;
esac
