{ pkgs, lib, ... }: let
in {
  imports = [ ./config.nix ];

  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      extraPackages = ps: with ps; [ psycopg2 ];
    };
  };

  services.postgresql = {
    ensureDatabases = ["hass"];
    ensureUsers = [{
      name = "hass";
      ensurePermissions = {
        "DATABASE hass" = "ALL PRIVILEGES";
      };
    }];
  };

  services.nginx = {
    virtualHosts."hass.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        proxy_buffering off;
      '';
      locations."/".extraConfig = ''
        proxy_pass http://127.0.0.1:8123;
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

  krops.secrets.files."home-assistant-secrets.yaml" = {
    owner = "hass";
    path = "/var/lib/hass/secrets.yaml";
  };
  users.users.hass.extraGroups = [ "keys" ];

  krops.secrets.files.home-assistant-ldap.owner = "hass";
  services.openldap.extraConfig = ''
    objectClass ( 1.3.6.1.4.1.28297.1.2.4 NAME 'homeAssistant'
            SUP uidObject AUXILIARY
            DESC 'Added to an account to allow home-assistant access'
            MUST (mail) )
  '';
}
