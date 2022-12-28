let
  notifyJoerg = msg: {
    service = "notify.pushover";
    data_template.message = msg;
  };
  notifyShannan = msg: {
    service = "notify.mobile_app_beatrice";
    data_template.message = msg;
  };
  presenceScript = {
    service = "python_script.presence";
    data_template = {
      from_state = "{{trigger.from_state.state}}";
      to_state = "{{trigger.to_state.state}}";
      entity_id = "{{trigger.entity_id}}";
    };
  };
in
{
  services.home-assistant.pythonScripts = ./python-scripts;
  services.home-assistant.config = {
    input_boolean.shannan_joerg_not_together = {
      name = "Shannan and Jörg are not together";
      icon = "mdi:account-switch";
    };
    sensor = [
      {
        platform = "template";
        sensors.distance_joerg_shannan = {
          friendly_name = "Distance between Jörg and Shannan";
          unit_of_measurement = "km";
          value_template = "{{ distance('person.jorg_thalheim', 'device_tracker.beatrice') }}";
        };
      }
    ];

    binary_sensor = [
      {
        platform = "template";
        sensors.different_locations_joerg_shannan = {
          friendly_name = "Jörg and Shannan are in different locations";
          value_template = "{{ states.person.jorg_thalheim.state !=  states.device_tracker.beatrice.state }}";
        };
      }
    ];

    automation = [
      {
        alias = "Set Shannan and Jörg are not together";
        trigger = [
          {
            platform = "numeric_state";
            entity_id = "sensor.distance_joerg_shannan";
            above = 1;
          }
          {
            platform = "state";
            entity_id = "binary_sensor.different_locations_joerg_shannan";
            to = "on";
            for = "00:00:25";
          }
        ];
        action = {
          service = "input_boolean.turn_on";
          entity_id = "input_boolean.shannan_joerg_not_together";
        };
      }
      {
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
      }
      {
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
      }
      {
        alias = "Shannan and Jörg are together";
        trigger = {
          platform = "numeric_state";
          entity_id = "sensor.distance_joerg_shannan";
          below = 0.2;
        };
        condition = {
          condition = "template";
          value_template = ''            {{ states("input_boolean.shannan_jorg_not_together") == "on" and
                                          states("person.jorg_thalheim") == "not_home" and
                                          states("device_tracker.beatrice") == "not_home" }}'';
        };
        action = [
          {
            service = "input_boolean.turn_off";
            entity_id = "input_boolean.shannan_joerg_not_together";
          }
          (
            notifyJoerg "Shannan is close to you {{ sensor.distance_joerg_shannan | round(2)}}km"
          )
          (
            notifyShannan "Jörg is close to you {{ sensor.distance_joerg_shannan | round(2)}}km"
          )
        ];
      }
    ];
  };
}
