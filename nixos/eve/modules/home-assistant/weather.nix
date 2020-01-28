 { ... }: {
   services.home-assistant.config = {
     script = {
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
       forecast = [ 0 ];
     }];
    automations = [{
      alias = "Rainy day notification";
      trigger = {
        platform = "state";
        entity_id = "sensor.dark_sky_precip_0d";
        to = "rain";
      };
      action.service = "script.notify_weather";
    }];
   };
 }
