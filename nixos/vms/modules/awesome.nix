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
    rofi
  ];

  services.autorandr.enable = true;
  programs.xss-lock.enable = true;
  programs.xss-lock.lockerCommand = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
  programs.nm-applet.enable = true;
}
