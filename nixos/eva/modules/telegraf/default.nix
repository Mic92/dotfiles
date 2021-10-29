{ config, lib, pkgs, ... }:

{
  imports = [
    ../../../modules/telegraf.nix
    ./private.nix
    ./uni.nix
    ./krebs.nix
    ./nix-community.nix
  ];

  services.nginx.virtualHosts."telegraf.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:8186/;
    '';
  };

  sops.secrets.telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  services.telegraf.environmentFiles = [
    config.sops.secrets.telegraf.path
  ];
}
