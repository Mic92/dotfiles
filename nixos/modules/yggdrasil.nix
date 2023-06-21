{ config, ... }: {
  # yggdrasil -genconf
  sops.secrets.yggdrasil-config = { };

  services.yggdrasil.enable = true;
  services.yggdrasil.configFile = config.sops.secrets.yggdrasil-config.path;
  networking.firewall.allowedTCPPorts = [
    12345
  ];
}
