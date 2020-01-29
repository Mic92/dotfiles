{ pkgs, lib, ... }: let
in {
  imports = [
    ./bike-light.nix
    ./charge-notifications.nix
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
    calendar = {
      platform = "caldav";
      url = "https://cloud.thalheim.io/remote.php/dav";
      username = "hass@thalheim.io";
      password = "!secret ldap_password";
      calendars = [
        "joergshannan_shared_by_joerg@higgsboson.tk"
      ];
    };
    sun = {};
    influxdb = {
      username = "homeassistant";
      host = "influxdb.thalheim.io";
      password = "!secret influxdb";
      database = "homeassistant";
      ssl = true;
      include.entities = [
        "person.jorg_thalheim"
        "person.dorit_thalheim"
        "person.falk_thalheim"
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
    device_tracker = [{
      platform = "fritz";
      host = "fritzbox.ohorn.thalheim.io";
      username = "home-assistant";
      password = "!secret fritzbox_password";
    } {
      platform = "fritz2";
      host = "fritzbox.ohorn.thalheim.io";
      username = "home-assistant";
      password = "!secret fritzbox_password";
    }];
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
