{
  services.home-assistant.config = {
    sensor = [
      {
        name = "random_joke";
        platform = "rest";
        json_attributes = "joke";
        resource = "https://icanhazdadjoke.com/";
        scan_interval = "3600";
        headers.Accept = "application/json";
      }
    ];

    intent_script.TellJoke = {
      speech.text = ''{{ state_attr("sensor.random_joke", "joke") }}'';
      action = {
        service = "homeassistant.update_entity";
        entity_id = "sensor.random_joke";
      };
    };
  };
}
