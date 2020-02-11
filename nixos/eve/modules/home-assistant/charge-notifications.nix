{ ... }: {
  services.home-assistant.config = {
    binary_sensor = [{
      platform = "trend";
      sensors.redmi_charging = {
        entity_id = "device_tracker.redmi_note_5";
        attribute = "battery_level";
      };
    }];
    automation = [{
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
        entity_id  = "device_tracker.shannans_apple_watch";
        value_template = "{{ state.attributes.battery }}";
        below = 30;
        for = "00:10:00";
      };
      condition = {
        condition = "template";
        value_template = ''{{ state_attr("device_tracker.shannans_apple_watch", "battery_status") == "NotCharging" }}'';
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch only got {{ state_attr("device_tracker.shannans_apple_watch", "battery") | round(1) }}% battery left'';
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
          value_template = ''{{ state_attr("device_tracker.shannans_apple_watch", "battery_status") == "Charging" }}'';
        }];
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch is still on charge ({{ state_attr("device_tracker.shannans_apple_watch", "battery") | round(1) }}%)'';
      }];
    } {
      alias = "Apple watch charged notification";
      trigger = {
        platform = "numeric_state";
        entity_id  = "device_tracker.shannans_apple_watch";
        value_template = "{{ state.attributes.battery }}";
        above = 95;
        for = "00:10:00";
      };
      condition = {
        condition = "template";
        value_template = ''{{ state_attr("device_tracker.shannans_apple_watch", "battery_status") != "NotCharging" }}'';
      };
      action = [{
        service = "notify.mobile_app_beatrice";
        data_template.message = ''Apple watch was charged up to {{ state_attr("device_tracker.shannans_apple_watch", "battery") | round(1) }}%'';
      }];
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
    }];
  };
}
