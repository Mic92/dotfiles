{ pkgs, lib, ... }:
{
  imports = [
    ./xserver.nix
    ./flameshot.nix
  ];
  services.xserver.windowManager = {
    i3.enable = true;
    default = "i3";
  };

  environment.systemPackages = with pkgs; [
    pavucontrol
    lightlocker
    lxappearance
    scrot
    evince
    rofi
    gnome3.eog
    libnotify
    dunst
    compton-git
    (i3pystatus.override {
      extraLibs = [ python3.pkgs.keyrings-alt ];
    })
    xlibs.xbacklight
    gnome3.networkmanagerapplet
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
