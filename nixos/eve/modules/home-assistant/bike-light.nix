{ ... }: let
  chargeNotification = place: {
     alias = "Charge bike light at ${place} notification";
     trigger = {
       platform = "state";
       entity_id = "person.jorg_thalheim";
       to = place;
       for = "00:20:00";
     };
     condition = {
       condition = "template";
       value_template = ''{{ states("input_number.days_bike_light_uncharged") | int >= 3 }}'';
     };
     action = [{
       service = "notify.pushover";
       data_template.message = ''Bike light was not charged since {{ states("input_number.days_bike_light_uncharged") | round(0) }} days'';
     }];
  };
in {
  services.home-assistant.config = {
    input_number.days_bike_light_uncharged = {
      name = "Days bike light uncharged";
      min = 0;
      max = 10;
      step = 1;
      icon = "mdi:lightbulb";
    };
    input_boolean.bike_light_used_today = {
      name = "Bike light used today";
      icon = "mdi:lightbulb";
    };
    automation = [
      (chargeNotification "University")
      (chargeNotification "home")
      (chargeNotification "Shannan's Home")
      {
        alias = "Increment discharged counter";
        trigger = {
          platform = "state";
          entity_id  = "person.jorg_thalheim";
          from = "University";
        };
        condition = {
          condition = "template";
          value_template = ''{{ states("sun.sun") == "below_horizon" and states("input_boolean.bike_light_used_today") == "off" }}'';
        };
        action = [{
          service = "input_number.increment";
          entity_id = "input_number.days_bike_light_uncharged";
        } {
          service = "input_boolean.turn_on";
          entity_id = "input_boolean.bike_light_used_today";
        }];
      } {
        alias = "Reset bike light used today bool";
        trigger = {
          platform = "time";
          at = "00:00:01";
        };
        action = [{
          service = "input_boolean.turn_off";
          entity_id = "input_boolean.bike_light_used_today";
        }];
      }];
  };
}
