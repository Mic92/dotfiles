{ config, ... }: {
  imports = [
    ./private.nix
    ./uni.nix
    ./krebs.nix
    ./nix-community.nix
    ./numtide.nix
  ];

  sops.secrets.eva-telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  services.telegraf.environmentFiles = [
    config.sops.secrets.eva-telegraf.path
  ];
}
