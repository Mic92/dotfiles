{ ... }: {
  services.home-assistant.config.homeassistant = {
    name = "Home";
    latitude = "!secret home_latitude";
    longitude = "!secret home_longitude";
    elevation = "!secret home_elevation";
    unit_system = "metric";
    time_zone = "Europe/Berlin";
  };
  services.home-assistant.config.zone = [{
    name = "Lidle";
    icon = "mdi:shopping";
    latitude = "!secret lidle_latitude";
    longitude = "!secret lidle_longitude";
    radius = "150";
  }
    {
      name = "University";
      icon = "mdi:school";
      latitude = "!secret uni_latitude";
      longitude = "!secret uni_longitude";
      radius = "300";
    }];
}
