{ pkgs, ... }: let
  ldap-auth-sh = pkgs.callPackage ./ldap-auth-sh.nix {};

  chargeNotification = place: {
    alias = "Charge bike light at ${place} notification";
    trigger = {
      platform = "state";
      entity_id = "person.jorg_thalheim";
      to = place;
      for = "00:20:00";
    };
    condition = {
      condition = "template";
      value_template = ''{{ states("input_number.days_bike_light_uncharged") >= 3 }}'';
    };
    action = [{
      service = "notify.pushover";
      data_template.message = ''Bike light was not charged since {{ states("input_number.days_bike_light_uncharged") | round(0) }} days'';
    }];
  };
in {
  services.home-assistant.config = {
    frontend = {};
    http = {};
    "map" = {};
    homeassistant = {
      name = "Joerg's Home";
      latitude = "!secret home_latitude";
      longitude = "!secret home_longitude";
      elevation = "!secret home_elevation";
      unit_system = "metric";
      time_zone = "Europe/London";

      auth_providers = [{
        type = "command_line";
        command = "${ldap-auth-sh}/bin/ldap-auth.sh";
        meta = true;
      }];
    };
    shopping_list = {};
    sun = {};
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
      platform = "template";
      sensors.shannan_joerg_distance = {
        value_template = ''{{ distance('person.jorg_thalheim', 'person.shannan_lekwati') | round(2) }}'';
        entity_id = [
          "person.jorg_thalheim"
          "person.shannan_lekwati"
        ];
      };
    } {
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
        "precip_probability"
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
    input_number.days_bike_light_uncharged = {
      name = "Days bike light uncharged";
      min = 0;
      max = 10;
      step = 1;
      icon = "mdi:lightbulb";
    };
    input_boolean.bike_light_used_today = {
      name = "Bike light used today";
      icon = "mdi:lightbulb";
    };
    script = {
      decrement_discharge_counter.sequence = [{
        service = "input_number.increment";
        entity_id = "input_number.days_bike_light_uncharged";
      } {
        service = "input_boolean.turn_on";
        entity_id = "input_boolean.bike_light_used_today";
      }];
      notify_weather.sequence = [{
        service = "notify.mobile_app_jorg_s_xiaomi";
        data_template = {
          title = "Weather";
          message = ''{{ states("sensor.dark_sky_summary_0d") }} (rain propability {{ states("sensor.dark_sky_precip_probability_0d") }}%)'';
          data.photo.url = ''https://hass.thalheim.io{{ state_attr("sensor.dark_sky_summary_0d", "entity_picture")}}'';
        };
      } {
        service = "notify.mobile_app_beatrice";
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
        service = "notify.mobile_app_beatrice";
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
      alias = "Shannan arrived home notification";
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
      alias = "Joerg arrived home notification";
      trigger = {
        platform = "state";
        entity_id  = "person.jorg_thalheim";
        to = "home";
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = "Jörg is home";
      }];
    } {
      alias = "Shannan left home notification";
      trigger = {
        platform = "state";
        entity_id  = "person.shannan_lekwati";
        from = "home";
      };
      action = [{
        service = "notify.pushover";
        data_template.message = "Shannan left home";
      }];
    } {
      alias = "Joerg left home notification";
      trigger = {
        platform = "state";
        entity_id  = "person.jorg_thalheim";
        from = "home";
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = "Jörg left home";
      }];
    } {
      alias = "Joerg arrived home notification";
      trigger = {
        platform = "state";
        entity_id  = "person.jorg_thalheim";
        to = "home";
      };
      action = [{
        service = "notify.mobile_app_beatrice";
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
        service = "notify.mobile_app_beatrice";
        data_template.message = "Jörg left Uni";
      }];
    } {
      alias = "Shannan left work notification";
      trigger = {
        platform = "state";
        entity_id  = "person.shannan_lekwati";
        from = "Work of Shannan";
        to = "not_home";
      };
      action = [{
        service = "notify.pushover";
        data_template.message = "Shannan left work";
      }];
    } {
      alias = "Maurice left Uni notification";
      trigger = {
        platform = "state";
        entity_id  = "person.maurice_baillieu";
        from = "University";
      };
      action = [{
        service = "notify.pushover";
        data_template.message = "Maurice left Uni";
      }];
    } {
      alias = "Maurice arrived at Uni notification";
      trigger = {
        platform = "state";
        entity_id  = "person.maurice_baillieu";
        to = "University";
      };
      action = [{
        service = "notify.pushover";
        data_template.message = "Maurice arrived at Uni";
      }];
    } {
      alias = "IPhone battery warning";
      trigger = {
        platform = "numeric_state";
        entity_id  = "device_tracker.beatrice";
        value_template = "{{ state.attributes.battery }}";
        below = 30;
        for = "00:10:00";
      };
      condition = {
        condition = "template";
        value_template = ''{{ state_attr("device_tracker.beatrice", "battery") == "NotCharging" }}'';
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Iphone only got {{ state_attr("device_tracker.beatrice", "battery") | round(1) }}% battery left'';
      }];
    } {
      alias = "Apple watch battery warning";
      trigger = {
        platform = "numeric_state";
        entity_id  = "device_tracker.shannansapple_watch";
        value_template = "{{ state.attributes.battery }}";
        below = 30;
        for = "00:10:00";
      };
      condition = {
        condition = "template";
        value_template = ''{{ state_attr("device_tracker.shannansapple_watch", "battery_status") == "NotCharging" }}'';
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch only got {{ state_attr("device_tracker.shannansapple_watch", "battery") | round(1) }}% battery left'';
      }];
    } {
      alias = "Apple watch wearing reminder notification";
      trigger = {
        platform = "time";
        at = "8:30:00";
      };
      condition = {
        condition = "and";
        conditions = [{
          condition = "time";
          weekday = [ "mon" "tue" "wed" "thu" "fri"];
        } {
          condition = "template";
          value_template = ''{{ state_attr("device_tracker.shannansapple_watch", "battery_status") != "NotCharging" }}'';
        }];
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch is still on charge ({{ state_attr("device_tracker.shannansapple_watch", "battery") | round(1) }}%)'';
      }];
    } {
      alias = "Apple watch charged notification";
      trigger = {
        platform = "numeric_state";
        entity_id  = "device_tracker.shannansapple_watch";
        value_template = "{{ state.attributes.battery }}";
        above = 95;
        for = "00:10:00";
      };
      condition = {
        condition = "template";
        value_template = ''{{ state_attr("device_tracker.shannansapple_watch", "battery_status") != "NotCharging" }}'';
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch was charged up to {{ state_attr("device_tracker.shannansapple_watch", "battery") | round(1) }}%'';
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
        data_template.message = ''Redmi was charged up {{ state_attr("device_tracker.redmi_note_5", "battery_level") }}%'';
      }];
    }
    (chargeNotification "University")
    (chargeNotification "home")
    (chargeNotification "Shannan's Home")
    {
      alias = "Increment discharged counter";
      trigger = {
        platform = "state";
        entity_id  = "person.jorg_thalheim";
        from = "University";
      };
      condition = {
        condition = "template";
        value_template = ''{{ states("sun.sun") == "below_horizon" and states("input_boolean.bike_light_used_today") == "off" }}'';
      };
      action.service = "script.decrement_discharge_counter";
    } {
      alias = "Reset bike light used today bool";
      trigger = {
        platform = "time";
        at = "00:00:01";
      };
      action = [{
        service = "input_boolean.turn_off";
        entity_id = "input_boolean.bike_light_used_today";
      }];
    }];
  };
}
