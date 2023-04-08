{
  services.home-assistant.config = {
    conversation.intents = {
      RoomTemperature = [
        "(How|What) is the (temperature|air quality|humidity)"
        "(How|What) is the (temperature|air quality|humidity) in the room"
      ];
    };
    intent_script.RoomTemperature.speech.text =
      "It is {{ states('sensor.bme680_temperature') | round(0) }} degrees inside. "
      + "The air quality is {{ states('sensor.bme680_air_quality') | round(0) }} percent and the humidity is {{ states('sensor.bme680_humidity') | round(0) }}.";
    automation = [
      {
        alias = "open a window notification";
        trigger = {
          platform = "numeric_state";
          entity_id = "sensor.bme680_air_quality";
          below = 75;
          for = "00:10:00";
        };
        action = [
          {
            service = "rest_command.tts";
            data_template.message = ''Open a window, please.'';
          }
        ];
      }
      {
        alias = "close the window notification";
        trigger = {
          platform = "numeric_state";
          entity_id = "sensor.bme680_temperature";
          below = 19;
          for = "00:10:00";
        };
        action = [
          {
            service = "rest_command.tts";
            data_template.message = ''Close the window, please.'';
          }
        ];
      }
    ];
  };
}
