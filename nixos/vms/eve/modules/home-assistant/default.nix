{ pkgs, lib, ... }: let
  ldap-auth-sh = pkgs.stdenv.mkDerivation {
    name = "ldap-auth-sh";
    src = pkgs.fetchFromGitHub {
      owner = "efficiosoft";
      repo = "ldap-auth-sh";
      rev = "93b2c00413942908139e37c7432a12bcb705ac87";
      sha256 = "1pymp6ki353aqkigr89g7hg5x1mny68m31c3inxf1zr26n5s2kz8";
    };
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      mkdir -p $out/etc
      cat > $out/etc/home-assistant.cfg << 'EOF'
      CLIENT="ldapsearch"
      SERVER="ldap://localhost:389"
      USERDN="cn=home-assistant,ou=system,ou=users,dc=eve"
      PW="$(cat /run/keys/home-assistant-ldap)"

      BASEDN="ou=users,dc=eve"
      SCOPE="subtree"
      FILTER="(&(objectClass=homeAssistant)(mail=$(ldap_dn_escape "$username")))"
      USERNAME_PATTERN='^[a-z|A-Z|0-9|_|-|.|@]+$'
      on_auth_success() {
        # print the meta entries for use in HA
        if echo "$output" | grep -qE '^(dn|DN):: '; then
            # ldapsearch base64 encodes non-ascii
            output=$(echo "$output" | sed -n -e "s/^\(dn\|DN\)\s*::\s*\(.*\)$/\2/p" | base64 -d)
        else
            output=$(echo "$output" | sed -n -e "s/^\(dn\|DN\)\s*:\s*\(.*\)$/\2/p")
        fi

        name=$(echo "$output" | sed -nr 's/^cn=([^,]+).*/\1/Ip')
        [ -z "$name" ] || echo "name=$name"
      }
      EOF
      install -D -m755 ldap-auth.sh $out/bin/ldap-auth.sh
      wrapProgram $out/bin/ldap-auth.sh \
        --prefix PATH : ${lib.makeBinPath [ pkgs.openldap pkgs.coreutils pkgs.gnused pkgs.gnugrep ]} \
        --add-flags "$out/etc/home-assistant.cfg"
    '';
  };
in {
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
        auth_providers = [{
          type = "command_line";
          command = "${ldap-auth-sh}/bin/ldap-auth.sh";
          meta = true;
        }];
      };
      homeassistant = {
        name = "Home";
        latitude = "!secret latitude";
        longitude = "!secret longitude";
        elevation = "!secret elevation";
        unit_system = "metric";
        time_zone = "Europe/London";
      };
      zone = [{
        name = "Elternhaus";
        icon = "mdi:home";
        latitude = "51.173370";
        longitude = "14.032305";
        radius = "100";
      } {
        name = "Shannan";
        icon = "mdi:human-female-girl";
        latitude = "55.9673361";
        longitude = "-3.1458731";
        radius = "100";
      } {
        name = "University";
        icon = "mdi:school";
        latitude = "55.9447576";
        longitude = "-3.1896472";
        radius = "200";
      }];
      influxdb = {
        username = "homeassistant";
        host = "influxdb.thalheim.io";
        password = "!secret influxdb";
        database = "homeassistant";
        ssl = true;
        include.entities = [ "device_tracker.redmi_note_5" ];
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
      cloud = {};
      system_health = {};
      binary_sensor = [{
        platform = "trend";
        sensors.redmi_charging = {
          entity_id = "device_tracker.redmi_note_5";
          attribute = "battery_level";
        };
      }];
      automation = [{
        alias = "Redmi battery warnings";

        trigger = {
          platform = "numeric_state";
          entity_id  = "device_tracker.redmi_note_5";
          value_template = "{{ state.attributes.battery_level }}";
          below = 30;
          for = "00:10:00";
        };
        condition = {
          condition = "template";
          value_template = ''{{ states("binary_sensor.redmi_charging") != "on"  }}'';
        };
        action = [{
          service = "notify.pushover";
          data_template = {
            message = ''Redmi only has {{ state_attr("device_tracker.redmi_note_5", "battery_level") }}% battery left'';
          };
        }];
      } {
        alias = "Redmi charged notification";
        trigger = {
          platform = "numeric_state";
          entity_id  = "device_tracker.redmi_note_5";
          value_template = "{{ state.attributes.battery_level }}";
          above = 95;
          for = "00:10:00";
        };
        condition = {
          condition = "template";
          value_template = ''{{ states("binary_sensor.redmi_charging") == "on"  }}'';
        };
        action = [{
          service = "notify.pushover";
          data_template = {
            message = ''Redmi was charged up {{ state_attr("device_tracker.redmi_note_5", "battery_level") }}%'';
          };
        }];
      }];
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
