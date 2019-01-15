{ pkgs, lib, ... }:

{
  imports = [
    ./common.nix
    ./rust.nix
    ./latex.nix
    ./debugging.nix
  ];

  fonts.fontconfig.enableProfileFonts = true;
  programs.emacs.package = lib.mkForce pkgs.emacs;

  services.syncthing.enable = true;
  home.packages = with pkgs; [
    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont

    arandr
    rambox
    albert
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
    gpodder
    ncmpcpp
    xclip
    screen-message
    alacritty
    sublime3

    sshfsFuse
    sshuttle
    jq
    httpie
    pypi2nix
    go2nix
    mercurial # go2nix
    gnupg1compat
    cheat
    tldr
    nixopsUnstable
  ] ++ (with nur.repos.mic92; [
    inxi
    conky-symbols
    source-code-pro-nerdfonts.overrideAttrs
  ]);
}
