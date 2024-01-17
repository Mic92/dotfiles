{ lib, config, ... }:
{
  clan.networking.zerotier.networkId = lib.mkDefault (builtins.readFile (config.clanCore.clanDir + "/machines/eve/facts/zerotier-network-id"));
  services.zerotierone.joinNetworks = [ "7c31a21e86f9a75c" ];

  services.zerotierone.localConf.settings = {
    interfacePrefixBlacklist = [ "tinc" "wiregrill" ];
  };

  clan.networking.zerotier.moon.orbitMoons = [ "267efd4a15" "a03b47494d" ];
}
