{ config, ... }:
{
  imports = [
    ./private.nix
    ./dave.nix
    ./http-sd.nix
    ./krebs.nix
    ./nixos-wiki-infra.nix
    ./uni.nix
  ];

  sops.secrets.eva-telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  services.telegraf.environmentFiles = [ config.sops.secrets.eva-telegraf.path ];
}
