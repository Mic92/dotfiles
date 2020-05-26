{ lib, ... }:
let
  notifyJoerg = msg: [{
    service = "notify.pushover";
    data_template.message = msg;
  }];
  notifyShannan = msg: [{
    service = "notify.mobile_app_beatrice";
    data_template.message = msg;
  }];

  person = entity_id: { to ? null, from ? null }: {
    platform = "state"; inherit entity_id;
  } // lib.optionalAttrs (to != null) { inherit to; }
  // lib.optionalAttrs (from != null) { inherit from; };

  joerg = person "person.jorg_thalheim";
  # only track one device to avoid duplicate notifications
  shannan = person "device_tracker.beatrice";
  maurice = person "person.maurice_bailleu";
  dimitra = person "person.dimitra";
  sasha = person "person.aleksandr_maramzin";
  gymTime = {
    condition = "template";
    value_template = ''{{
      state_attr("calendar.joerg_shannan_jorg_thalheim", "message") == "Gym" and
      states("binary_sensor.shannan_jorg_not_together")
    }}'';
  };
  notTogether = {
    condition = "template";
    value_template = ''{{ states("binary_sensor.shannan_jorg_not_together") == "on" }}'';
  };
in
{
  services.home-assistant.config.binary_sensor = [{
    name = "shannan_jörg_not_together";
    platform = "threshold";
    entity_id = "sensor.average_distance_jorg_shannan";
    upper = 0.5;
    hysteresis = 0.2;
  }];

  services.home-assistant.config.sensor = [{
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
  } {
    platform = "filter";
    name = "average distance Jörg - Shannan";
    entity_id = "sensor.distance_joerg_shannan";
    filters = [{
      filter = "time_simple_moving_average";
      window_size = "00:35";
      precision = "2";
    }];
  }];
  services.home-assistant.config.automation = [{
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
    alias = "Joerg at Shannan's notification";
    trigger = joerg { to = "Shannan's Home"; };
    condition = notTogether;
    action = notifyShannan "Jörg arrived at your place";
  } {
    alias = "Shannan at Joerg's place notification";
    trigger = shannan { to = "home"; };
    condition = notTogether;
    action = notifyJoerg "Shannan arrived at your place";
  } {
    alias = "Shannan arrived home notification";
    trigger = shannan { to = "Shannan's Home"; };
    condition = notTogether;
    action = notifyJoerg "Shannan is home";
  } {
    alias = "Joerg arrived home notification";
    trigger = joerg { to = "home"; };
    condition = notTogether;
    action = notifyShannan "Jörg is home";
  } {
    alias = "Shannan left home notification";
    trigger = shannan { from = "Shannan's Home"; };
    condition = notTogether;
    action = notifyJoerg "Shannan left home";
  } {
    alias = "Joerg left home notification";
    trigger = joerg { from = "home"; };
    condition = notTogether;
    action = notifyShannan "Jörg left home";
  } {
    alias = "Joerg left Uni notification";
    trigger = joerg { from = "University"; };
    condition = notTogether;
    action = notifyShannan "Jörg left Uni";
  } {
    alias = "Shannan left work notification";
    trigger = shannan { from = "Work of Shannan"; };
    condition = notTogether;
    action = notifyJoerg "Shannan left work";
  } {
    alias = "Joerg arrived at the Gym notification";
    trigger = joerg { to = "Gym"; };
    condition = gymTime;
    action = notifyShannan "Joerg arrived at the Gym";
  } {
    alias = "Shannan arrived at the Gym notification";
    trigger = shannan { to = "Gym"; };
    condition = gymTime;
    action = notifyJoerg "Shannan arrived at the Gym";
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
}
