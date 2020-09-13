{ pkgs, ... }: {
  # Define a user account
  users.extraUsers.kodi.isNormalUser = true;

  # allow everybody in the net to access the wifi
  networking.firewall = {
    allowedTCPPorts = [ 8080 ];
    allowedUDPPorts = [ 8080 ];
  };

  environment.systemPackages = [
    pkgs.mpv
  ];

  hardware.pulseaudio.enable = true;
  services.avahi = {
    enable = true;
    publish.enable = true;
    publish.userServices = true;
  };

  services.cage.program = "${pkgs.kodi-wayland}/bin/kodi-standalone";
  services.cage.user = "kodi";
  services.cage.enable = true;
}
