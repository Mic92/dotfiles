{ pkgs, ... }: {
  # allow everybody in the net to access the wifi
  networking.firewall.allowedTCPPorts = [ 8096 ];

  services.jellyfin.enable = true;

  environment.systemPackages = [
    pkgs.jellyfin-mpv-shim
  ];

  hardware.pulseaudio.enable = true;
}
