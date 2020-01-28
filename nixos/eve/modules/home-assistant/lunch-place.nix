{ ... }: {
  services.home-assistant.config = {
    sensor = [{
      name = "choose_place";
      platform = "rest";
      json_attributes = "places";
      resource = "https://choose-place.herokuapp.com/api/occasion/lunch";
    } {
      platform = "template";
      sensors = let
        choice = num: {
          value_template = "{{ state_attr('sensor.choose_place', 'places')[${toString num}]['name'].title() }}";
          entity_id = "sensor.choose_place";
          friendly_name = "${toString (num + 1)}.";
        };
      in {
        first_lunch_choice = choice 0;
        second_lunch_choice = choice 1;
        third_lunch_choice = choice 2;
      };
    }];
    automation = [{
      alias = "Lunch place options";
      trigger = {
        platform = "time";
        at = "12:00:00";
      };
      action = [{
        service = "notify.pushover";
        data_template = {
          message = ''
          Lunch options:
            {{ states("sensor.first_lunch_choice") }}

            {{ states("sensor.second_lunch_choice") }}

            {{ states("sensor.third_lunch_choice") }}
          '';
        };
      }];
      condition = {
        condition = "template";
        value_template = ''{{ states("person.jorg_thalheim") == "University" }}'';
      };
    }];
  };
}
