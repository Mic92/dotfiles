{ pkgs, ... }: {
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
}
