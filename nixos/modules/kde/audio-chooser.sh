#!/usr/bin/nix-shell
#!nix-shell -i bash -p fuzzel pamixer pulseaudio
#shellcheck shell=bash
set -exuo pipefail

choices=("headphones" "speakers" "headphones (output-only)" "earphones")
selected=$(printf "\n%s" "${choices[@]}" | fuzzel -d)

echo You Picked: " $selected"

headset=alsa_card.usb-0b0e_Jabra_Link_380_08C8C2E6DA47-00
speakers=alsa_card.pci-0000_00_1f.3
earphones=bluez_output.50_C2_75_67_67_8C.1
address="50:C2:75:67:67:8C"

case $selected in
headphones)
  echo "Setting up Headphones"
  bluetoothctl disconnect "5C:56:A4:74:38:19" || true
  pactl set-card-profile "$headset" output:analog-stereo+input:mono-fallback || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$earphones" off || true
  ;;
speakers)
  echo "Setting up Speakers"
  bluetoothctl disconnect "$address" || true
  pactl set-card-profile "$speakers" output:analog-stereo+input:analog-stereo || true
  pactl set-card-profile "$headset" off || true
  pactl set-card-profile "$earphones" off || true
  ;;
"headphones (output-only)")
  echo "Setting up Headphones (Output-Only)"
  bluetoothctl disconnect "$address" || true
  pactl set-card-profile "$headset" output:analog-stereo || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$earphones" off || true
  ;;
"earphones")
  bluetoothctl connect "$address"
  echo "Setting up Earphones"
  pactl set-card-profile "$earphones" a2dp-sink || true
  pactl set-card-profile "$headset" off || true
  pactl set-card-profile "$speakers" off || true
  ;;
*)
  echo "Invalid Option $selected"
  exit 1
  ;;
esac

pactl list short sinks | while read -r sink; do
  sink_idx=$(echo "$sink" | cut -f1)
  pamixer --unmute --sink "$sink_idx" || true
done

pactl list short sources | while read -r source; do
  source_idx=$(echo "$source" | cut -f1)
  pamixer --unmute --source "$source_idx" || true
done
