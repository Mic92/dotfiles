{
  services.zerotierone.joinNetworks = [
    "ccc5da5295c853d4"
    "b15644912e61dbe0"
  ];

  services.zerotierone.localConf.settings = {
    interfacePrefixBlacklist = [
      "tinc"
      "wiregrill"
      "hyprspace"
    ];
  };
}
