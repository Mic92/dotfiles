{
  # Enable X11 windowing system
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "modesetting" ];

  # Enable Kodi
  services.xserver.desktopManager.kodi.enable = true;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "kodi";

  # Define a user account
  users.extraUsers.kodi.isNormalUser = true;

  # allow everybody in the net to access the wifi
  networking.firewall = {
    allowedTCPPorts = [ 8080 ];
    allowedUDPPorts = [ 8080 ];
  };
}
