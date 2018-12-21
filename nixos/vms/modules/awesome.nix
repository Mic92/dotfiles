{ pkgs, ... }: {
  imports = [ ./xserver.nix ];
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
    xautolock
  ];

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
