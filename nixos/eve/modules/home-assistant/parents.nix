{ ... }: {
  services.home-assistant.config = {
    influxdb.include.entities = [
      "person.dorit_thalheim"
      "person.falk_thalheim"
    ];
    device_tracker = [{
      platform = "fritz";
      host = "fritzbox.ohorn.thalheim.io";
      username = "home-assistant";
      password = "!secret fritzbox_password";
    } {
      platform = "fritz2";
      host = "fritzbox.ohorn.thalheim.io";
      username = "home-assistant";
      password = "!secret fritzbox_password";
    }];
    zone = [{
      name = "Thalheim's Home";
      icon = "mdi:home";
      latitude = "!secret elternhaus_latitude";
      longitude = "!secret elternhaus_longitude";
      radius = "100";
    }];
  };
}
