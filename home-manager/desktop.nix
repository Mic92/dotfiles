{ pkgs
, inputs
, ...
}: {
  imports = [
    ./common.nix
    ./modules/atuin-autosync.nix
    ./modules/rust.nix
    ./modules/debugging.nix
    ./modules/default-apps.nix
    ./modules/waybar.nix
  ];

  fonts.fontconfig.enable = true;

  programs.vscode.enable = true;

  services.mpris-proxy.enable = true;
  services.syncthing.enable = true;

  home.packages = with pkgs; [
    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    twitter-color-emoji
    upterm
    eternal-terminal
    gimp

    arandr
    signal-desktop
    inputs.nur-packages.packages.${pkgs.hostPlatform.system}.pandoc-bin
    gnome.adwaita-icon-theme
    hicolor-icon-theme
    graphicsmagick
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    dino
    foot
    screen-message
    sshfs-fuse
    sshuttle
    jq
    git-lfs
    cheat
    xdg-utils
    patool
    tio
    shell_gpt

    (mpv.override {
      scripts = [ mpvScripts.mpris ];
    })
    playerctl
    yt-dlp
    mumble
    ferdium
    kubectl
    hyperfine
    inkscape

    q
    rbw
    isync
    # to fix xdg-open
    glib
    zoom-us
    jmtpfs # needed for charging? WTF
    (pkgs.runCommand "slack-aliases" { } ''
      mkdir -p $out/bin
      declare -A rooms=([numtide]=numtide \
             ["numtide-labs"]="numtide-labs" \
             ["tum"]="ls1-tum" \
             ["tum-courses"]="ls1-tum-course")
      for name in "''${!rooms[@]}"; do
        cat > "$out/bin/slack-''${name}" <<EOF
      #!${runtimeShell}
      exec chromium --app="https://''${rooms[$name]}.slack.com" "$@"
      EOF
      done
      chmod +x $out/bin/slack-*
    '')
    (pkgs.writeScriptBin "jellyfinmediaplayer" ''
      # bluetooth speaker
      bluetoothctl connect E6:4D:D6:0A:CC:9B &
      systemd-inhibit \
        --why="Jellyfin Media Player" \
        --who="Jellyfin Media Player" \
        --mode=block \
        ${pkgs.jellyfin-media-player}/bin/jellyfinmediaplayer
    '')
    (retroarch.override {
      cores = [
        libretro.bsnes-hd
        libretro.mupen64plus
        libretro.beetle-psx-hw
        libretro.dolphin
        #libretro.pcsx2
      ];
    })

    (pkgs.writeScriptBin "rhasspy-play" ''
      #!${pkgs.runtimeShell}
      set -eux -o pipefail
      export PATH=${pkgs.pulseaudioFull}/bin:$PATH
      sink=alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink

      if pamixer --get-mute --sink="$sink"; then
          pamixer --sink=$sink --unmute
          paplay --device=$sink
          pamixer --sink=$sink --mute
      else
          paplay --device=$sink
      fi
    '')

    nixos-shell
  ]
  ++ (with inputs.nur-packages.packages.${pkgs.hostPlatform.system}; [
    speedscope
    inxi
    source-code-pro-nerdfonts
    (pkgs.nerdfonts.override {
      fonts = [ "FiraCode" ];
    })
  ]);
}
