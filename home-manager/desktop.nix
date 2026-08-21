{
  self,
  pkgs,
  inputs,
  lib,
  ...
}:
{
  imports = [
    ./common.nix
    ./modules/atuin-autosync.nix
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/kimai.nix
    ./modules/mail.nix
    ./modules/radicle.nix
  ];

  # herdr-eternal target for eve; authenticate with:
  #   herdr-eternal-ssh login eve
  xdg.configFile."herdr-eternal/config.toml".text = ''
    [targets.eve]
    url = "wss://herdr.thalheim.io/herdr-eternal"
    quic_addr = "herdr.thalheim.io:7443"
    issuer = "https://auth.thalheim.io"
    client_id = "herdr-eternal"
    forward_agent = true
  '';

  fonts.fontconfig.enable = true;

  services.mpris-proxy.enable = true;
  services.syncthing.enable = true;

  home.packages =
    with pkgs;
    [
      # Roaming transport for herdr --remote (used via remote.ssh_command).
      inputs.herdr-eternal.packages.${pkgs.stdenv.hostPlatform.system}.default
      league-of-moveable-type
      dejavu_fonts
      ubuntu-classic
      unifont
      twitter-color-emoji
      upterm
      gimp
      (symlinkJoin {
        name = "signal-desktop";
        paths = [ signal-desktop ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/signal-desktop --add-flags --password-store=kwallet6
        '';
      })
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
      yt-dlp
      mumble
      ferdium
      kubectl
      hyperfine

      q
      rbw
      self.packages.${pkgs.stdenv.hostPlatform.system}.rbw-pinentry
      # to fix xdg-open
      glib
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
      inter
      nerd-fonts.fira-code
      nerd-fonts.jetbrains-mono
      inxi
      inputs.niks3.packages.${pkgs.stdenv.hostPlatform.system}.niks3
    ]
    ++ lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux") [
      # terminfo conflict with ncurses
      (lib.hiPrio ghostty)
    ];
}
