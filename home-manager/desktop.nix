{ self, pkgs, ... }:
{
  imports = [
    ./common.nix
    ./modules/atuin-autosync.nix
    ./modules/calendar.nix
    ./modules/claude-mcp.nix
    ./modules/mail.nix
  ];

  fonts.fontconfig.enable = true;

  services.mpris-proxy.enable = true;
  services.syncthing.enable = true;

  home.packages =
    with pkgs;
    [
      league-of-moveable-type
      dejavu_fonts
      ubuntu_font_family
      unifont
      twitter-color-emoji
      upterm
      eternal-terminal
      gimp
      signal-desktop
      adwaita-icon-theme
      hicolor-icon-theme
      graphicsmagick
      screen-message
      sshfs-fuse
      sshuttle
      jq
      git-lfs
      cheat
      xdg-utils
      tio

      (mpv.override { scripts = [ mpvScripts.mpris ]; })
      playerctl
      yt-dlp
      mumble
      ferdium
      kubectl
      hyperfine

      q
      rbw
      # to fix xdg-open
      glib
      zoom-us
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
      self.packages.${pkgs.stdenv.hostPlatform.system}.gmaps-cli
      self.packages.${pkgs.stdenv.hostPlatform.system}.kagi-search
    ]
    ++ lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux") [
      self.inputs.ghostty.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];
}
