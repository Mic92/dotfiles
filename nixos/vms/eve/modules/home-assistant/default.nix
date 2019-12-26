{ pkgs, lib, ... }: {
  imports = [
    ./ldap.nix
    ./phone.nix
  ];
  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      extraPackages = ps: with ps; [ psycopg2 ];
    };
    config = {
      frontend = {};
      http = {};
      "map" = {};
      homeassistant = {
        name = "Home";
        latitude = "!secret latitude";
        longitude = "!secret longitude";
        elevation = "!secret elevation";
        unit_system = "metric";
        time_zone = "Europe/London";
      };
      influxdb = {
        username = "homeassistant";
        host = "influxdb.thalheim.io";
        password = "!secret influxdb";
        database = "homeassistant";
        ssl = true;
      };
      notify = [{
        name = "Pushover";
        platform = "pushover";
        api_key = "!secret pushover_api_key";
        user_key = "!secret pushover_user_key";
      }];
      recorder.db_url = "postgresql://@/hass";
      config = {};
      mobile_app = {};
      system_health = {};
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
}
