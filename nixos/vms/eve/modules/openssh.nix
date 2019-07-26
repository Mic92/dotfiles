{ config, ... }: {
  services.openssh = {
    enable = true;
    ports = [
      22
      22022 # legacy
    ];
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };
  networking.firewall.allowedTCPPorts = config.services.openssh.ports;

  services.netdata.portcheck.checks = {
    ssh.port = 22;
    ssh-legacy.port = 22022;
  };
}
