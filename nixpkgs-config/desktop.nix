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
in {
  imports = [
    ./common.nix
    ./modules/rust.nix
    #./modules/latex.nix
    ./modules/debugging.nix
    ./modules/default-apps.nix
    ./modules/bitwarden.nix
  ];

  manual.json.enable = true;

  fonts.fontconfig.enable = true;

  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "SauceCodePro Nerd Font Mono 8";
        alignment = "left";
        geometry = "0x5-3+29";
        corner_radius = "2";
        padding = "8";
        horizontal_padding = "8";
        frame_width = "1";
        frame_color = "#dbdbdb";
        markup = "full";
        transparency = "10";
      };
      urgency_low = {
        background = "#303030";
        foreground = "#888888";
        timeout = "10";
      };
      urgency_normal = {
        background = "#303030";
        foreground = "#c7c7c7";
        timeout = "10";
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = "0";
      };
    };
  };

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
    #gimp
    firefox
    #chromium
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    urlview
    dino
    xorg.xev
    xorg.xprop
    xclip
    gpodder
    ncmpcpp
    xclip
    screen-message
    alacritty
    sshfsFuse
    sshuttle
    jq
    httpie
    pypi2nix
    go2nix
    nix-index
    gnupg1compat
    cheat
    tldr
    nixpkgs-pytools
    hydra-check

    (mpv.override {
      scripts = [ mpvScripts.mpris ];
    })
    wmc-mpris
    playerctl
    youtube-dl

    isync
    mu
    # to fix xdg-open
    glib

  ] ++ (with nur.repos.mic92; [
    speedscope
    inxi
    source-code-pro-nerdfonts
  ]);
}
