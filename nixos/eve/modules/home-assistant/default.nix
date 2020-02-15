{ pkgs, lib, ... }: let
in {
  imports = [
    ./bike-light.nix
    ./charge-notifications.nix
    #./parents.nix
    ./ldap.nix
    ./location-notifications.nix
    ./lunch-place.nix
    ./postgres.nix
    ./weather.nix
    ./zones.nix
  ];

  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      extraPackages = ps: with ps; [ psycopg2 ];
    };
  };

  services.home-assistant.config = {
    frontend = {};
    http = {};
    "map" = {};
    shopping_list = {};
    sun = {};
    calendar = {
      platform = "caldav";
      url = "https://cloud.thalheim.io/remote.php/dav";
      username = "hass@thalheim.io";
      password = "!secret ldap_password";
    };
    influxdb = {
      username = "homeassistant";
      host = "influxdb.thalheim.io";
      password = "!secret influxdb";
      database = "homeassistant";
      ssl = true;
      include.entities = [
        "person.jorg_thalheim"
        "person.shannan_lekwati"
        "device_tracker.beatrice"
        "device_tracker.redmi_note_5"
      ];
    };
    notify = [{
      name = "Pushover";
      platform = "pushover";
      api_key = "!secret pushover_api_key";
      user_key = "!secret pushover_user_key";
    }];
    config = {};
    mobile_app = {};
    icloud = {
      username = "!secret icloud_email";
      password = "!secret icloud_password";
      account_name = "Shannan's icloud";
    };
    cloud = {};
    system_health = {};
    sensor = [{
      platform = "template";
      sensors.shannan_joerg_distance = {
        value_template = ''{{ distance('person.jorg_thalheim', 'person.shannan_lekwati') | round(2) }}'';
        entity_id = [
          "person.jorg_thalheim"
          "person.shannan_lekwati"
        ];
      };
      sensors.joerg_last_updated = {
        friendly_name = "Jörg's last location update";
        value_template = ''{{ states.person.jorg_thalheim.last_updated.strftime('%Y-%m-%dT%H:%M:%S') }}Z'';
        entity_id = "person.jorg_thalheim";
        device_class = "timestamp";
      };
      sensors.shannan_last_updated = {
        friendly_name = "Shannan's last location update";
        value_template = ''{{ states.person.shannan_lekwati.last_updated.strftime('%Y-%m-%dT%H:%M:%S') }}Z'';
        entity_id = "person.shannan_lekwati";
        device_class = "timestamp";
      };
    }];
  };

  services.nginx = {
    virtualHosts."hass.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        # rsa4096 certificate for android
        ssl_certificate /var/lib/acme/hass.thalheim.io/fullchain.pem;
        ssl_certificate_key /var/lib/acme/hass.thalheim.io/key.pem;

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

  security.acme.certs."hass.thalheim.io" = {
    webroot = "/var/lib/acme/acme-challenge";
    postRun = "systemctl reload nginx.service";
    allowKeysForGroup = true;
    group = "nginx";
    keyType = "rsa4096";
  };

  krops.secrets.files."home-assistant-secrets.yaml" = {
    owner = "hass";
    path = "/var/lib/hass/secrets.yaml";
  };

  users.users.hass.extraGroups = [ "keys" ];
}
