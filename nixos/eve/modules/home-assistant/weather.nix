 { ... }: {
   services.home-assistant.config = {
     script = {
       notify_weather.sequence = let
         data_template = {
           title = "Weather";
           message = ''{{ states("sensor.dark_sky_summary_0h") }} (rain propability {{ states("sensor.dark_sky_precip_probability_0h") }}%)'';
           data.photo.url = ''https://hass.thalheim.io{{ state_attr("sensor.dark_sky_summary_0h", "entity_picture")}}'';
         };
       in [{
         service = "notify.mobile_app_jorg_s_xiaomi";
         inherit data_template;
       } {
         service = "notify.mobile_app_beatrice";
         inherit data_template;
       } {
         service = "input_boolean.turn_on";
         entity_id = "input_boolean.rain_notified_today";
       }];
     };
     input_boolean.rain_notified_today = {
       name = "Rain notified today";
       icon = "mdi:weather-cloudy";
     };
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
         "precip_probability"
       ];
       hourly_forecast = [ 0 ];
     }];
     automation = [{
       alias = "Rainy day notification";
       trigger = {
         platform = "state";
         entity_id = "sensor.dark_sky_precip_0h";
         to = "rain";
       };
       action.service = "script.notify_weather";
    } {
      alias = "Reset rain notified today";
      trigger = {
        platform = "time";
        at = "00:00:01";
      };
      action = [{
        service = "input_boolean.turn_off";
        entity_id = "input_boolean.rain_notified_today";
      }];
    }];
   };
 }
