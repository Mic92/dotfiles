{pkgs, ...}: {
  users.extraUsers = {
    dashboard = {
      isNormalUser = true;
      home = "/home/dashboard";
      extraGroups = ["plugdev" "input"];
    };
  };

  networking.firewall.allowedTCPPorts = [
    3030 # api
  ];

  services.xserver = {
    displayManager.lightdm.autoLogin.enable = true;
    displayManager.lightdm.autoLogin.user = "dashboard";
    displayManager.lightdm.autoLogin.timeout = 3;
    videoDrivers = ["modesetting"];
    desktopManager.xfce.extraSessionCommands = ''
      firefox http://127.0.0.1:3030 &
      xdotool search --sync --onlyvisible --class "Firefox" windowactivate key F11 &
      unclutter -idle 60 &
    '';
  };

  environment.systemPackages = let
    office-dashboard = import (builtins.fetchTarball "https://github.com/Mic92/office-dashboard/archive/master.tar.gz#2") {
      inherit pkgs;
    };
  in
    with pkgs; [
      office-dashboard
      firefox-bin
      xdotool
      unclutter
    ];
}
