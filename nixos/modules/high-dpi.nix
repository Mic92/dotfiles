{ lib
, pkgs
, ...
}: {
  hardware.video.hidpi.enable = true;
  services.xserver.displayManager.setupCommands = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --dpi eDP-1
  '';

  environment.variables = {
    _JAVA_OPTIONS = lib.mkDefault "-Dsun.java2d.uiScale=2";
    GDK_SCALE = lib.mkDefault "2";
    GDK_DPI_SCALE = lib.mkDefault "0.5";
  };
}
