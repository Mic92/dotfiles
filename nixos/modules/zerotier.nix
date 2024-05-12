{ lib, config, ... }:
{
  clan.networking.zerotier.networkId = lib.mkDefault (
    builtins.readFile (config.clanCore.clanDir + "/machines/eve/facts/zerotier-network-id")
  );
  services.zerotierone.joinNetworks = [
    "ccc5da5295c853d4"
    "b15644912e61dbe0"
  ];

  services.zerotierone.localConf.settings = {
    interfacePrefixBlacklist = [
      "tinc"
      "wiregrill"
    ];
  };

  clan.networking.zerotier.moon.orbitMoons = [
    "267efd4a15"
    "a03b47494d"
  ];
}
