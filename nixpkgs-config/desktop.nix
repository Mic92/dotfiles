{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
    ./rust.nix
    ./latex.nix
    ./debugging.nix
    ./bitwarden.nix
  ];

  fonts.fontconfig.enable = true;

  programs.emacs.imagemagick.enable = true;

  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "SauceCodePro Nerd Font Mono 12";
        alignment = "left";
        geometry = "0x5-3+29";
        corner_radius = "2";
        padding = "8";
        horizontal_padding = "8";
        frame_width = "1";
        frame_color = "#dbdbdb";
        markup = "full";
        format = "<b>%s</b>\n%b";
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

  services.syncthing.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Musik";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "Pulse output"
      }
    '';
  };
  home.packages = with pkgs; [
    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    twitter-color-emoji

    arandr
    signal-desktop
    copyq
    nur.repos.mic92.pandoc-bin
    gnome3.defaultIconTheme
    hicolor_icon_theme
    graphicsmagick
    gimp
    mpv
    youtube-dl
    firefox
    thunderbird
    chromium
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
    keepassx-community
    #(pass.withExtensions (e: [ e.pass-import ]))
    #rofi-pass
    #browserpass
    #qtpass
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
    gnupg1compat
    cheat
    tldr

    isync
    mu
    # to fix xdg-open
    glib

    rubber
    (texlive.combine {
      inherit (texlive)
      scheme-full

      # awesome cv
      xetex
      unicode-math
      ucharcat
      collection-fontsextra
      fontspec

      collection-binextra
      collection-fontsrecommended
      collection-latex
      collection-latexextra
      collection-latexrecommended
      collection-langgerman
      siunitx
      bibtex
      tracklang
      IEEEtran
      algorithm2e;
    })
  ] ++ (with nur.repos.mic92; [
    inxi
    conky-symbols
    source-code-pro-nerdfonts
    ferdi
  ]);
}
