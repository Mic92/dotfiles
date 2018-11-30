{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./rust.nix
    ./latex.nix
    ./debugging.nix
  ];

  fonts.fontconfig.enableProfileFonts = true;

  services.syncthing.enable = true;
  home.packages = with pkgs; [
    albert
    arandr

    rambox
    albert

    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    gnome3.defaultIconTheme
    hicolor_icon_theme
    graphicsmagick
    sshfsFuse
    sshuttle
    gimp
    mpv
    youtube-dl
    firefox
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
  ] ++ (with nur.repos.mic92; [
    inxi
    conky-symbols
    inconsolata-nerdfonts
  ]);
}
