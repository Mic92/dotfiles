{ config, ... }: {
  services.shadowsocks = {
    enable = true;
    passwordFile = config.sops.secrets.shadowsocks.path;
  };
  networking.firewall.allowedTCPPorts = [ 8388 ];
}
