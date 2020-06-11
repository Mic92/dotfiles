{
   services.home-assistant.config = {
     input_boolean.rain_notified_today = {
       name = "Rain notified today";
       icon = "mdi:weather-cloudy";
     };
     weather = {
       platform = "openweathermap";
       api_key = "!secret openweathermap_api_key";
     };
     automation = [{
       alias = "rainy/snowy day notification";
       trigger = {
         platform = "state";
         entity_id = "weather.openweathermap";
       };
       condition = {
         condition = "template";
         value_template = ''{{ states.input_boolean.rain_notified_today.state == "off" }}'';
       };
       action = [{
         service = "python_script.weather";
       }];
     } {
       alias = "Reset rain notified today";
       trigger = {
         platform = "time";
         at = "00:07:00";
       };
       action = [{
         service = "input_boolean.turn_off";
         entity_id = "input_boolean.rain_notified_today";
       }];
     }];
   };
}
