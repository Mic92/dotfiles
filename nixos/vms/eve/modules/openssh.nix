{ config, ... }: {
  services.openssh = {
    enable = true;
    ports = [
      22022 # legacy
      22
    ];
  };
  networking.firewall.allowedTCPPorts = config.services.openssh.ports;
}
