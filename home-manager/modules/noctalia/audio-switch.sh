# shellcheck shell=bash
# Routes audio to exactly one card and turns all others off, so
# pipewire's automatic port switching can't pick the wrong device.
# `audio-switch list` feeds the noctalia launcher (name<TAB>card),
# `audio-switch <line>` activates the chosen device.

# name|card|profile|bluetooth address
devices=(
  "audeze|alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01|output:analog-stereo+input:mono-fallback|"
  "speakers|alsa_card.pci-0000_c1_00.6|output:analog-stereo+input:analog-stereo|"
  "audeze (output-only)|alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01|output:analog-stereo|"
  "earphones|bluez_output.50_C2_75_67_67_8C.1|a2dp-sink|50:C2:75:67:67:8C"
  "audeze (bt)|bluez_card.E0_49_ED_04_B7_B4|a2dp-sink|E0:49:ED:04:B7:B4"
  "beats|bluez_card.0C_53_B7_B5_B2_49|a2dp-sink|0C:53:B7:B5:B2:49"
)
# Picks up too much noise to ever be useful.
webcam=alsa_card.usb-046d_C505e_HD_Webcam_C614D880-02

if [[ ${1:-} == list ]]; then
  for d in "${devices[@]}"; do
    IFS='|' read -r name card _ _ <<<"$d"
    printf '%s\t%s\n' "$name" "$card"
  done
  exit 0
fi

selected=${1%%$'\t'*}
target=
for d in "${devices[@]}"; do
  [[ ${d%%|*} == "$selected" ]] && target=$d
done
if [[ -z $target ]]; then
  notify-send -a audio-switch "Audio" "Unknown device: $selected"
  exit 1
fi
IFS='|' read -r _ target_card target_profile target_bt <<<"$target"

pactl set-card-profile "$webcam" off || true

declare -A seen_bt seen_card
for d in "${devices[@]}"; do
  IFS='|' read -r _ card _ bt <<<"$d"
  if [[ -n $bt && -z ${seen_bt[$bt]:-} ]]; then
    seen_bt[$bt]=1
    if [[ $bt == "$target_bt" ]]; then
      bluetoothctl connect "$bt" || true
    else
      bluetoothctl disconnect "$bt" || true
    fi
  fi
  if [[ $card != "$target_card" && -z ${seen_card[$card]:-} ]]; then
    seen_card[$card]=1
    # Bluetooth cards vanish when disconnected, so this may fail.
    pactl set-card-profile "$card" off || true
  fi
done

if ! err=$(pactl set-card-profile "$target_card" "$target_profile" 2>&1); then
  notify-send -a audio-switch -u critical "Audio" "$selected: $err"
  exit 1
fi

pactl list short sinks | cut -f1 | xargs -r -n1 pamixer --unmute --sink
pactl list short sources | cut -f1 | xargs -r -n1 pamixer --unmute --source
notify-send -a audio-switch "Audio" "$selected connected"
