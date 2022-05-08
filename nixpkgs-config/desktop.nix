{
  pkgs,
  lib,
  config,
  ...
}: let
  vscodeExtensions =
    (with pkgs.vscode-extensions; [
      bbenoist.Nix
      ms-python.python
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
      ms-vscode.cpptools
      vscodevim.vim
    ])
    ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "remote-ssh-edit";
        publisher = "ms-vscode-remote";
        version = "0.50.0";
        sha256 = "1b2lqd89vnynbzd3rss1jahc1zxs769s921rclgy1v7z1sd1kqxy";
      }
    ];
in {
  imports = [
    ./common.nix
    ./modules/rust.nix
    #./modules/kak.nix
    ./modules/debugging.nix
    ./modules/default-apps.nix
  ];

  manual.json.enable = true;

  fonts.fontconfig.enable = true;

  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = ["network.target" "sound.target"];
    Install.WantedBy = ["default.target"];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Service.Restart = "always";
  };

  services.syncthing.enable = true;

  home.packages = with pkgs;
    [
      #(vscode-with-extensions.override {
      #  inherit vscodeExtensions;
      #})

      league-of-moveable-type
      dejavu_fonts
      ubuntu_font_family
      unifont
      twitter-color-emoji

      arandr
      signal-desktop
      nur.repos.mic92.pandoc-bin
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
      go2nix
      nix-index
      cheat
      hydra-check
      xdg_utils
      patool

      (mpv.override {
        scripts = [mpvScripts.mpris];
      })
      wmc-mpris
      playerctl
      yt-dlp
      mumble
      ferdi
      msmtp
      kubectl
      terraform-ls

      rbw
      isync
      # to fix xdg-open
      glib
      zoom-us
      jmtpfs # needed for charging? WTF
      #
      (pkgs.callPackage ./pkgs/mpv-tv.nix {})
      (pkgs.runCommand "slack-aliases" {} ''
        mkdir -p $out/bin
        declare -A rooms=([manifold]=manifoldfinance \
               [numtide]=numtide \
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
    ++ (with nur.repos.mic92; [
      q
      speedscope
      inxi
      source-code-pro-nerdfonts
      (pkgs.nerdfonts.override {
        fonts = ["FiraCode"];
      })
    ]);

  systemd.user.timers.mbsync = {
    Unit.Description = "mbsync";
    Timer.OnBootSec = "10m";
    Timer.OnUnitInactiveSec = "10m";
    Timer.Unit = "mbsync.service";
    Install.WantedBy = ["timers.target"];
  };

  systemd.user.services.mbsync = {
    Service.Type = "oneshot";
    Service.Environment = "PATH=${pkgs.rbw}/bin";
    # TODO: Fix ini generator in home-manager
    Service.ExecStart = ''
      -${pkgs.isync}/bin/mbsync -a --quiet
      ExecStart=${pkgs.emacs}/bin/emacsclient -e (mu4e-update-index)
    '';
  };
}
