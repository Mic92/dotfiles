{ pkgs, lib, ... }:
{
  imports = [
    ./xserver.nix
    ./flameshot.nix
  ];
  services.xserver.windowManager = {
    awesome.enable = true;
    awesome.luaModules = with pkgs.lua52Packages; [
      luasocket
      cjson
    ];
  };
  services.xserver.displayManager.defaultSession = "none+awesome";

  environment.systemPackages = with pkgs; [
    pavucontrol
    lightlocker
    lxappearance
    scrot
    evince
    rofi
    gnome3.eog
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
