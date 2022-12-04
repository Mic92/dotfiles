{
  services.home-assistant.config.homeassistant = {
    name = "Home";
    latitude = "!secret home_latitude";
    longitude = "!secret home_longitude";
    elevation = "!secret home_elevation";
    unit_system = "metric";
    time_zone = "Europe/Berlin";
  };
  services.home-assistant.config.zone = [
    {
      name = "University";
      icon = "mdi:school";
      latitude = "!secret uni_latitude";
      longitude = "!secret uni_longitude";
      radius = "300";
    }
    {
      name = "Grunecker";
      icon = "mdi:briefcase";
      latitude = "!secret work_latitude";
      longitude = "!secret work_longitude";
      radius = "300";
    }
    {
      name = "Parents";
      icon = "mdi:human-male-female-child";
      latitude = "!secret parents_latitude";
      longitude = "!secret parents_longitude";
      radius = "200";
    }
  ];
}
