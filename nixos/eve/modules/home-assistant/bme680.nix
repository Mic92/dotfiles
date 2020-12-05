{
  services.home-assistant.config = {
    intent_script.RoomTemperature.speech.text = ''
      It is {{ states('sensor.bme680_temperature') | round(0) }} degrees inside. The air quality is {{ states('sensor.bme680_air_quality') | round(0) }} percent.
    '';
     automation = [{
       alias = "open a window notification";
       trigger = {
         platform = "numeric_state";
         entity_id  = "sensor.bme680_air_quality";
         below = 75;
         for = "00:10:00";
       };
       action = [{
         service = "notify.pushover";
         data_template.message = "Open a window!";
       }];
     }];
  };
}
