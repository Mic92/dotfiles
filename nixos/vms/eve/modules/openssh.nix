{ config, ... }: {
  services.openssh = {
    enable = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
    passwordAuthentication = false;
    listenAddresses = [
      { addr = "0.0.0.0"; port = 22; }
      { addr = "0.0.0.0"; port = 22022; } # legacy
      { addr = "[::]"; port = 22; }
      { addr = "[::]"; port = 22022; } # legacy
      { addr = "[2a01:4f9:2b:1605::2]"; port = 443; }
    ];
  };
  networking.firewall.allowedTCPPorts = [ 22 22022 443 ];

  services.netdata.portcheck.checks = {
    ssh.port = 22;
    ssh-legacy.port = 22022;
  };
}
