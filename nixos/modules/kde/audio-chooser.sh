#!/usr/bin/nix-shell
#!nix-shell -i bash -p fuzzel pamixer pulseaudio libnotify bluez
#shellcheck shell=bash
set -exuo pipefail

choices=("headphones" "speakers" "headphones (output-only)" "earphones" "soundbar")
selected=$(printf "\n%s" "${choices[@]}" | fuzzel -d)

echo You Picked: " $selected"

headset=alsa_card.usb-0b0e_Jabra_Link_380_08C8C2E6DA47-00
speakers=alsa_card.pci-0000_00_1f.3
earphones=bluez_output.50_C2_75_67_67_8C.1
address="50:C2:75:67:67:8C"
soundbar=alsa_card.usb-DELL_DELL_Slim_Soundbar_SB522A_0-02

notify() {
  notify-send -a audio-chooser "Audio Chooser" "$1"
}

case $selected in
headphones)
  bluetoothctl disconnect "5C:56:A4:74:38:19" || true
  pactl set-card-profile "$headset" output:analog-stereo+input:mono-fallback || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$earphones" off || true
  pactl set-card-profile "$soundbar" off || true
  notify "Headphones Connected"
  ;;
speakers)
  bluetoothctl disconnect "$address" || true
  pactl set-card-profile "$speakers" output:analog-stereo+input:analog-stereo || true
  pactl set-card-profile "$headset" off || true
  pactl set-card-profile "$earphones" off || true
  pactl set-card-profile "$soundbar" off || true
  notify "Speakers Connected"
  ;;
"headphones (output-only)")
  bluetoothctl disconnect "$address" || true
  pactl set-card-profile "$headset" output:analog-stereo || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$earphones" off || true
  pactl set-card-profile "$soundbar" off || true
  notify "Headphones (Output-Only) Connected"
  ;;
"earphones")
  bluetoothctl connect "$address"
  pactl set-card-profile "$earphones" a2dp-sink || true
  pactl set-card-profile "$headset" off || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$soundbar" off || true
  notify "Earphones Connected"
  ;;
"soundbar")
  bluetoothctl disconnect "$address" || true
  pactl set-card-profile "$soundbar" output:analog-stereo || true
  pactl set-card-profile "$headset" off || true
  pactl set-card-profile "$speakers" off || true
  pactl set-card-profile "$earphones" off || true
  notify "Soundbar Connected"
  ;;
*)
  echo "Invalid Option $selected"
  notify "Invalid Option $selected"
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
