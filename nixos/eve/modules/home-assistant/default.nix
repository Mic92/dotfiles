{ pkgs, ... }: {
  imports = [
    ./bluetooth.nix
    ./bme680.nix
    ./charge-notifications.nix
    ./find-phone.nix
    ./jokes.nix
    ./laptops.nix
    ./light.nix
    ./ldap.nix
    ./noops.nix
    ./mqtt.nix
    ./presence.nix
    ./postgres.nix
    ./android.nix
    ./timer.nix
    ./transmission.nix
    ./vlc.nix
    ./weather.nix
    ./zones.nix
  ];

  services.home-assistant = {
    enable = true;
    package =
      (pkgs.home-assistant.override {
        extraPackages = ps: [
          ps.psycopg2
        ];
      });
  };

  services.home-assistant.config =
    let
      hiddenEntities = [
        "sensor.last_boot"
        "sensor.date"
      ];
    in
    {
      icloud = { };
      frontend = { };
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
      history.exclude = {
        entities = hiddenEntities;
        domains = [
          "automation"
          "updater"
        ];
      };
      "map" = { };
      shopping_list = { };
      backup = { };
      logbook.exclude.entities = hiddenEntities;
      logger.default = "info";
      sun = { };
      calendar = {
        platform = "caldav";
        url = "https://cloud.thalheim.io/remote.php/dav";
        username = "hass@thalheim.io";
        password = "!secret ldap_password";
      };
      prometheus.filter.include_domains = [
        "persistent_notification"
      ];
      notify = [
        {
          name = "Pushover";
          platform = "pushover";
          api_key = "!secret pushover_api_key";
          user_key = "!secret pushover_user_key";
        }
      ];
      device_tracker = [
        {
          platform = "luci";
          host = "rauter.r";
          username = "!secret openwrt_user";
          password = "!secret openwrt_password";
        }
      ];
      config = { };
      mobile_app = { };

      #icloud = {
      #  username = "!secret icloud_email";
      #  password = "!secret icloud_password";
      #  with_family = true;
      #};
      cloud = { };
      network = { };
      zeroconf = { };
      system_health = { };
      default_config = { };
      system_log = { };
      vlc_telnet = { };
      sensor = [
        {
          platform = "template";
          sensors.shannan_joerg_distance.value_template = ''{{ distance('person.jorg_thalheim', 'person.shannan_lekwati') | round(2) }}'';
          sensors.joerg_last_updated = {
            friendly_name = "Jörg's last location update";
            value_template = ''{{ states.person.jorg_thalheim.last_updated.strftime('%Y-%m-%dT%H:%M:%S') }}Z'';
            device_class = "timestamp";
          };
          sensors.shannan_last_updated = {
            friendly_name = "Shannan's last location update";
            value_template = ''{{ states.person.shannan_lekwati.last_updated.strftime('%Y-%m-%dT%H:%M:%S') }}Z'';
            device_class = "timestamp";
          };
        }
      ];
    };

  services.nginx.virtualHosts."hass.thalheim.io" = {
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

  sops.secrets."home-assistant-secrets.yaml" = {
    owner = "hass";
    path = "/var/lib/hass/secrets.yaml";
    restartUnits = [ "home-assistant.service" ];
  };
}
