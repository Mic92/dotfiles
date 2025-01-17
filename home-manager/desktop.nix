{ pkgs, lib, ... }:
let

  jj-fzf-src = pkgs.fetchFromGitHub {
    owner = "tim-janik";
    repo = "jj-fzf";
    rev = "b81c9d61e4a91679f88171793143e893e6e37421";
    sha256 = "sha256-Lz05VXfks6buUE2Poula6lg+Yw9OjS0q3LWafzsibdQ=";
  };
  jj-fzf =
    pkgs.runCommand "jj-fzf"
      {
        nativeBuildInputs = [
          pkgs.makeWrapper
          pkgs.bashInteractive
        ];
      }
      ''
        mkdir -p $out/bin
        install -m755 ${jj-fzf-src}/jj-fzf $out/bin/jj-fzf
        wrapProgram $out/bin/jj-fzf \
          --prefix PATH : ${
            lib.makeBinPath [
              pkgs.fzf
              pkgs.jujutsu
              pkgs.gnused
              pkgs.gawk
              pkgs.coreutils
              pkgs.bashInteractive
            ]
          }
      '';
in
{
  imports = [
    ./common.nix
    ./modules/atuin-autosync.nix
    ./modules/rust.nix
    ./modules/debugging.nix
    ./modules/default-apps.nix
    #./modules/waybar.nix
  ];

  fonts.fontconfig.enable = true;

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
    zed-editor
    jujutsu
    jj-fzf
    arandr
    signal-desktop
    adwaita-icon-theme
    hicolor-icon-theme
    graphicsmagick
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    dino
    ghostty
    screen-message
    sshfs-fuse
    sshuttle
    jq
    git-lfs
    cheat
    xdg-utils
    patool
    tio
    shell-gpt

    (mpv.override { scripts = [ mpvScripts.mpris ]; })
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

    nixos-shell
    nerd-fonts.fira-code
    inxi
  ];
}
