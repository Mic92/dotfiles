{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/telegraf.nix
    ./private.nix
    ./uni.nix
    ./krebs.nix
    ./it4r.nix
    ./nix-community.nix
  ];

  sops.secrets.telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  services.telegraf.environmentFiles = [
    config.sops.secrets.telegraf.path
  ];
}
