{ pkgs, lib, config, ... }:

let
  vscodeExtensions = (with pkgs.vscode-extensions; [
    bbenoist.Nix
    ms-python.python
    ms-azuretools.vscode-docker
    ms-vscode-remote.remote-ssh
    ms-vscode.cpptools
    vscodevim.vim
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
    name = "remote-ssh-edit";
    publisher = "ms-vscode-remote";
    version = "0.50.0";
    sha256 = "1b2lqd89vnynbzd3rss1jahc1zxs769s921rclgy1v7z1sd1kqxy";
  }];
in
{
  imports = [
    ./common.nix
    ./modules/rust.nix
    #./modules/latex.nix
    ./modules/debugging.nix
    ./modules/default-apps.nix
  ];

  manual.json.enable = true;

  fonts.fontconfig.enable = true;

  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = [ "network.target" "sound.target" ];
    Install.WantedBy = [ "default.target" ];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Service.Restart = "always";
  };

  services.syncthing.enable = true;

  home.packages = with pkgs; [
    #(vscode-with-extensions.override {
    #  inherit vscodeExtensions;
    #})

    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    twitter-color-emoji

    arandr
    xlibs.xkill
    signal-desktop
    nur.repos.mic92.pandoc-bin
    gnome3.defaultIconTheme
    hicolor_icon_theme
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
    sshfsFuse
    sshuttle
    jq
    go2nix
    nix-index
    cheat
    hydra-check
    xdg_utils

    (mpv.override {
      scripts = [ mpvScripts.mpris ];
    })
    wmc-mpris
    playerctl
    youtube-dl
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
  ] ++ (with nur.repos.mic92; [
    speedscope
    inxi
    source-code-pro-nerdfonts
    (pkgs.nerdfonts.override {
      fonts = [ "FiraCode" ];
    })
  ]);

  systemd.user.timers.mbsync = {
    Unit.Description = "mbsync";
    Timer.OnBootSec = "10m";
    Timer.OnUnitInactiveSec = "10m";
    Timer.Unit = "mbsync.service";
    Install.WantedBy = [ "timers.target" ];
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
