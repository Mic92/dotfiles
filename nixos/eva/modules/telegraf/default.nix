{ config, lib, pkgs, ... }:

{
  sops.secrets.telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  sops.secrets.telegraf-shared = {
    owner = config.systemd.services.telegraf.serviceConfig.User;
    sopsFile = ../../../secrets/telegraf.yaml;
  };
  systemd.services.telegraf.serviceConfig.SupplementaryGroups = [ "keys" ];

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

  services.telegraf = {
    environmentFiles = [
      config.sops.secrets.telegraf.path
      config.sops.secrets.telegraf-shared.path
    ];
    extraConfig = {
      agent.interval = lib.mkForce "200s";
      inputs = {
        influxdb_v2_listener = [{
          service_address = ":8186";
          token = ''''${INFLUXDB_PASSWORD}'';
        }];
      };
    };
  };
}
