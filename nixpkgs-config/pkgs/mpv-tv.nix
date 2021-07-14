{ fzf, coreutils, mpv, lib, fetchurl, writeShellScriptBin, gnused, curl }:
writeShellScriptBin "mpv-tv" ''
  set -eux -o pipefail
  export PATH=${lib.makeBinPath [ fzf coreutils mpv curl gnused ]}
  stream=$(curl https://raw.githubusercontent.com/jnk22/kodinerds-iptv/master/iptv/kodi/kodi_tv.m3u | sed '/#EXTM3U/d;/#EXTINF/{s/\r\n//g; s/.*,//g}' \
    | paste -d'\t' - - | fzf -d '\t' --with-nth 1 | cut -f2)
  exec mpv --force-window=yes "$stream"
''
