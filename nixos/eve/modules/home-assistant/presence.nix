{ lib, ... }:
let
  notifyJoerg = msg: {
    service = "notify.pushover";
    data_template.message = msg;
  };
  notifyShannan = msg: {
    service = "notify.mobile_app_beatrice";
    data_template.message = msg;
  };
  person = entity_id: { to ? null, from ? null }: {
    platform = "state"; inherit entity_id;
  } // lib.optionalAttrs (to != null) { inherit to; }
  // lib.optionalAttrs (from != null) { inherit from; };

  # only track one device to avoid duplicate notifications
  maurice = person "person.maurice_bailleu";
  dimitra = person "person.dimitra";
  sasha = person "person.aleksandr_maramzin";
  presenceScript = {
    service = "python_script.presence";
    data_template = {
      from_state = "{{trigger.from_state.state}}";
      to_state = "{{trigger.to_state.state}}";
      entity_id = "{{trigger.entity_id}}";
    };
  };
in {
  services.home-assistant.pythonScripts = ./python-scripts;
  services.home-assistant.config = {
    input_boolean.shannan_joerg_not_together = {
       name = "Shannan and Jörg are not together";
      icon = "mdi:account-switch";
    };
    sensor = [{
      platform = "template";
      sensors.distance_joerg_shannan = {
        entity_id = [
          "person.jorg_thalheim"
          "device_tracker.beatrice"
        ];
        friendly_name = "Distance between Jörg and Shannan";
        unit_of_measurement = "km";
        value_template = "{{ distance('person.jorg_thalheim', 'device_tracker.beatrice') }}";
      };
    }];
    automation = [{
      alias = "Set Shannan and Jörg are not together";
      trigger = {
        platform = "numeric_state";
        entity_id = "sensor.distance_joerg_shannan";
        above = 1;
      };
      action = {
        service = "input_boolean.turn_on";
        entity_id = "input_boolean.shannan_joerg_not_together";
      };
    } {
      alias = "Location notifications to Shannan";
      trigger = {
        platform = "state";
        entity_id = "person.jorg_thalheim";
      };
      action = presenceScript;
      condition = {
        condition = "template";
        value_template = ''{{ trigger.from_state.state != trigger.to_state.state }}'';
      };
    } {
      alias = "Location notifications to Jörg";
      trigger = {
        platform = "state";
        entity_id = "device_tracker.beatrice";
      };
      action = presenceScript;
      condition = {
        condition = "template";
        value_template = ''{{ trigger.from_state.state != trigger.to_state.state }}'';
      };
    } {
      alias = "Shannan and Jörg are together";
      trigger = {
        platform = "numeric_state";
        entity_id = "sensors.distance_joerg_shannan";
        below = 0.2;
      };
      condition = {
        condition = "template";
        value_template = ''{{ states("input_boolean.shannan_jorg_not_together") == "on" and
                              states("person.jorg_thalheim") == "not_home" and
                              states("device_tracker.beatrice") == "not_home" }}'';
      };
      action = [{
        service = "input_boolean.turn_off";
        entity_id = "input_boolean.shannan_joerg_not_together";
      } (
        notifyJoerg "Shannan is close to you {{ sensors.distance_joerg_shannan | round(2)}}km"
      ) (
        notifyShannan "Jörg is close to you {{ sensors.distance_joerg_shannan | round(2)}}km"
      )] ;
    } {
      alias = "Reset ping-tracker desktop";
      trigger = {
        platform = "homeassistant";
        event = "start";
      };
      action = [{
        service = "device_tracker.see";
        data = {
          host_name = "sascha_desktop";
          mac = "a0:8c:fd:f1:86:af";
          location_name = "Unknown";
        };
      } {
        service = "device_tracker.see";
        data = {
          host_name = "dimitra-desktop";
          mac = "6c:2b:59:8a:d3:3a";
          location_name = "Unknown";
        };
      } {
        service = "device_tracker.see";
        data = {
          host_name = "idontcare";
          mac = "00:0e:c6:e2:11:fa";
          location_name = "Unknown";
        };
      }];
    } {
      alias = "Maurice left Uni notification";
      trigger = maurice { to = "not_home"; from = "University"; };
      action = notifyJoerg "Maurice left Uni";
    } {
      alias = "Maurice arrived at Uni notification";
      trigger = maurice { from = "not_home"; to = "University"; };
      action = notifyJoerg "Maurice arrived at Uni";
    } {
      alias = "Dimitra left Uni notification";
      trigger = dimitra { to = "not_home"; from = "University"; };
      action = notifyJoerg "Dimitra left Uni";
    } {
      alias = "Dimitra arrived at Uni notification";
      trigger = dimitra { from = "not_home"; to = "University"; };
      action = notifyJoerg "Dimitra attrived at Uni";
    } {
      alias = "Sasha left Uni notification";
      trigger = sasha { to = "not_home"; from = "University"; };
      action = notifyJoerg "Sasha left Uni";
    } {
      alias = "Sasha arrived at Uni notification";
      trigger = sasha { from = "not_home"; to = "University"; };
      action = notifyJoerg "Sasha arrived at Uni";
    }];
  };
}
