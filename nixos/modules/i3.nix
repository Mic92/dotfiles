{ pkgs, lib, ... }:
{
  imports = [
    ./xserver.nix
    ./flameshot.nix
  ];
  services.xserver.windowManager.i3.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";

  environment.systemPackages = with pkgs; [
    # autostart stuff
    dex
    brightnessctl
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
    pamixer
    mpc_cli
    clipit
    (i3pystatus.override {
      extraLibs = [ python3.pkgs.keyrings-alt ];
    })
    gnome3.networkmanagerapplet
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
