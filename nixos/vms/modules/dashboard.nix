{ pkgs, ... }: {

  users.extraUsers = {
    dashboard = {
      isNormalUser = true;
      home = "/home/dashboard";
      extraGroups = [ "plugdev" "input" ];
    };
  };

  services.xserver = {
    displayManager.slim.autoLogin = true;
    displayManager.slim.defaultUser = "dashboard";
    videoDrivers = [ "modesetting" ];
    desktopManager.xfce.extraSessionCommands = ''
      firefox http://10.243.29.168:3030 &
      xdotool search --sync --onlyvisible --class "Firefox" windowactivate key F11 &
      unclutter -idle 60 &
    '';
  };

  environment.systemPackages = with pkgs; [
    firefox
    xdotool
    unclutter
  ];
}
