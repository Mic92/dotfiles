# shellcheck shell=bash
# Switches between phone tethering and the normal autoconnect profile.
# `net-switch list` feeds the noctalia launcher (name<TAB>description),
# `net-switch <line>` activates the choice.

if [[ ${1:-} == list ]]; then
  printf '%s\t%s\n' \
    bluetooth "Tether via phone (Bluetooth PAN)" \
    phone-wifi "Tether via phone hotspot" \
    default "Drop tethers, return to autoconnect"
  exit 0
fi

selected=${1%%$'\t'*}
case $selected in
bluetooth) up=("Pixel 8 Network") ;;
phone-wifi) up=("the.network") ;;
default)
  up=()
  nmcli connection down "the.network" || true
  nmcli connection down "Pixel 8 Network" || true
  ;;
*)
  notify-send -a net-switch "Network" "Unknown choice: $selected"
  exit 1
  ;;
esac

for c in "${up[@]}"; do
  if ! err=$(nmcli connection up "$c" 2>&1); then
    notify-send -a net-switch -u critical "Network" "$selected: $err"
    exit 1
  fi
done
notify-send -a net-switch "Network" "$selected active"
