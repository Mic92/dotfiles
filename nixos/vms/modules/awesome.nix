{ pkgs, lib, ... }:
{
  imports = [
    ./xserver.nix
    ./xss-lock.nix
  ];
  services.xserver.windowManager = {
    awesome = {
      enable = true;
      luaModules = with pkgs.lua52Packages; [ luasocket cjson ];
    };
    default = "awesome";
  };

  environment.systemPackages = with pkgs; [
    pavucontrol
    lxappearance
    scrot
    evince
    rofi
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
