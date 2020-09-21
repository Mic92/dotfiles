{ pkgs, ... }: {
  # allow everybody in the net to access the wifi
  networking.firewall.allowedTCPPorts = [ 8096 ];

  services.jellyfin.enable = true;

  environment.systemPackages = [
    pkgs.jellyfin-mpv-shim
  ];

  services.cage.enable = true;
  services.cage.user = "mpv";
  services.cage.program = "${pkgs.jellyfin-mpv-shim}/bin/jellyfin-mpv-shim";
  users.users.mpv.isNormalUser = true;

  hardware.pulseaudio.enable = true;
}
