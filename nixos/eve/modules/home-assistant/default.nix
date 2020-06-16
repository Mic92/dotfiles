{ pkgs, lib, ... }: let
in {
  imports = [
    ./bike-light.nix
    ./charge-notifications.nix
    #./parents.nix
    ./find-phone.nix
    ./german.nix
    ./jokes.nix
    ./light.nix
    ./ldap.nix
    ./ps4.nix
    ./lunch-place.nix
    ./noops.nix
    ./presence.nix
    ./postgres.nix
    ./timer.nix
    ./weather.nix
    ./zones.nix
  ];

  services.home-assistant = {
    enable = true;
    package = (pkgs.home-assistant.override {
      extraPackages = ps: with ps; [
        psycopg2
        (pkgs.python3.pkgs.callPackage ./coronavirus.nix {})
      ];
    }).overrideAttrs (old: {
      # nobody got time for that!
      doCheck = false;
      doInstallCheck = false;
    });
  };

  services.home-assistant.config = let
    hiddenEntities = [
      "sensor.first_lunch_choice"
      "sensor.second_lunch_choice"
      "sensor.third_lunch_choice"
      "sensor.choose_place"
      "sensor.last_boot"
      "sensor.date"
    ];
  in {
    coronavirus = {};
    frontend = {};
    http = {};
    history.exclude = {
      entities = hiddenEntities;
      domains = [
        "automation"
        "updater"
      ];
    };
    "map" = {};
    shopping_list = {};
    logbook.exclude.entities = hiddenEntities;
    logger.default = "info";
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
      with_family = true;
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

  krops.secrets."home-assistant-secrets.yaml" = {
    owner = "hass";
    path = "/var/lib/hass/secrets.yaml";
  };

  users.users.hass.extraGroups = [ "keys" ];

  services.netdata.httpcheck.checks.home-assistant = {
    url = "https://hass.thalheim.io";
    regex = "Home Assistant";
  };

  services.icinga2.extraConfig = ''
    apply Service "Homeassistant v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "hass.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Homeassistant v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "hass.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
