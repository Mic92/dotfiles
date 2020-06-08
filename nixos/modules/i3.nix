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
    pamixer
    mpc_cli
    clipit
    picom
    (i3pystatus.override {
      extraLibs = [ python3.pkgs.keyrings-alt ];
    })
    gnome3.networkmanagerapplet
    gnome3.file-roller
    gnome3.nautilus
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
