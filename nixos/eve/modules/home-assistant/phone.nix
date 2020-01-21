{ ... }: {
  services.home-assistant.config = {
    influxdb.include.entities = [ "device_tracker.redmi_note_5" ];
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
}
