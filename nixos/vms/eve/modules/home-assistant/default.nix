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
        name = "Joerg's Home";
        latitude = "!secret home_latitude";
        longitude = "!secret home_longitude";
        elevation = "!secret home_elevation";
        unit_system = "metric";
        time_zone = "Europe/London";
      };
      shopping_list = {};
      zone = [{
        name = "Thalheim's Home";
        icon = "mdi:home";
        latitude = "!secret elternhaus_latitude";
        longitude = "!secret elternhaus_longitude";
        radius = "100";
      } {
        name = "Shannan's Home";
        icon = "mdi:human-female-girl";
        latitude = "!secret shannan_latitude";
        longitude = "!secret shannan_longitude";
        radius = "100";
      } {
        name = "Work of Shannan";
        icon = "mdi:office-building";
        latitude = "!secret shannan_work_latitude";
        longitude = "!secret shannan_work_longitude";
        radius = "100";
      } {
        name = "University";
        icon = "mdi:school";
        latitude = "!secret uni_latitude";
        longitude = "!secret uni_longitude";
        radius = "200";
      } {
        name = "Gym";
        icon = "mdi:weight-lifter";
        latitude = "!secret gym_latitude";
        longitude = "!secret gym_longitude";
        radius = "100";
      }];
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
        ];
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
      device_tracker = [{
        platform = "icloud";
        username = "slekwati@outlook.com";
        password = "!secret icloud_password";
        account_name = "Shannan's icloud";
      } {
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
        platform = "darksky";
        api_key = "!secret darksky_api_key";
        monitored_conditions = [
          "summary"
          "icon"
          "temperature"
          "temperature_high"
          "temperature_low"
          "sunrise_time"
          "sunset_time"
          "alerts"
          "precip_type"
        ];
        forecast = [ 0 ];
      } {
        name = "choose_place";
        platform = "rest";
        json_attributes = "places";
        resource = "https://choose-place.herokuapp.com/api/occasion/lunch";
      } {
        platform = "template";
        sensors = let
          choice = num: {
            value_template = "{{ state_attr('sensor.choose_place', 'places')[${toString num}]['name'].title() }}";
            entity_id = "sensor.choose_place";
            friendly_name = "${toString (num + 1)}.";
          };
        in {
          first_lunch_choice = choice 0;
          second_lunch_choice = choice 1;
          third_lunch_choice = choice 2;
        };
      }];
      binary_sensor = [{
        platform = "trend";
        sensors.redmi_charging = {
          entity_id = "device_tracker.redmi_note_5";
          attribute = "battery_level";
        };
      }];
      script = {
        notify_weather.sequence = [{
          service = "notify.pushover";
          data_template = {
            title = "Weather";
            message = ''{{ states("sensor.dark_sky_summary_0d") }}'';
            data.attachment = ''https://hass.thalheim.io{{ state_attr("sensor.dark_sky_summary_0d", "entity_picture")}}'';
          };
        } {
          service = "notify.mobile_app_beatrice_2";
          data_template = {
            title = "Weather";
            message = ''{{ states("sensor.dark_sky_summary_0d") }}'';
            data.photo.url = ''https://hass.thalheim.io{{ state_attr("sensor.dark_sky_summary_0d", "entity_picture")}}'';
          };
        }];
      };
      automation = [{
        alias = "Joerg at Shannan's notification";
        trigger = {
          platform = "state";
          entity_id  = "person.jorg_thalheim";
          to = "Shannan's Home";
        };
        action = [{
          service = "notify.mobile_app_beatrice_2";
          data_template.message = "Jörg arrived at your place";
        }];
      } {
        alias = "Shannan at Joerg's place notification";
        trigger = {
          platform = "state";
          entity_id  = "person.shannan_lekwati";
          to = "home";
        };
        action = [{
          service = "notify.pushover";
          data_template.message = "Shannan arrived at your place";
        }];
      } {
        alias = "Shannan arrived home";
        trigger = {
          platform = "state";
          entity_id  = "person.shannan_lekwati";
          to = "Shannan's Home";
        };
        action = [{
          service = "notify.pushover";
          data_template.message = "Shannan is home";
        }];
      } {
        alias = "Joerg arrived home";
        trigger = {
          platform = "state";
          entity_id  = "person.jorg_thalheim";
          to = "home";
        };
        action = [{
          service = "notify.mobile_app_beatrice_2";
          data_template.message = "Jörg is home";
        }];
      } {
        alias = "Joerg left Uni notification";
        trigger = {
          platform = "state";
          entity_id  = "person.jorg_thalheim";
          from = "University";
        };
        action = [{
          service = "notify.mobile_app_beatrice_2";
          data_template.message = "Jörg left Uni";
        }];
      } {
        alias = "Shannan left work notification";
        trigger = {
          platform = "state";
          entity_id  = "person.shannan_lekwati";
          from = "Work of Shannan";
        };
        action = [{
          service = "notify.pushover";
          data_template.message = "Shannan is at work";
        }];
      } {
        alias = "Rainy day notification";
        trigger = {
          platform = "state";
          entity_id = "sensor.dark_sky_precip_0d";
          to = "rain";
        };
        action.service = "script.notify_weather";
      } {
        alias = "Lunch place options";
        trigger = {
          platform = "time";
          at = "12:00:00";
        };
        action = [{
          service = "notify.pushover";
          data_template = {
            message = ''
            Lunch options:
              {{ states("sensor.first_lunch_choice") }}

              {{ states("sensor.second_lunch_choice") }}

              {{ states("sensor.third_lunch_choice") }}
            '';
          };
        }];
        condition = {
          condition = "template";
          value_template = ''{{ states("person.jorg_thalheim") == "University" }}'';
        };
      } {
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
