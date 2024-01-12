{ lib, config, ... }:
{
  clan.networking.zerotier.networkId = lib.mkDefault (builtins.readFile (config.clanCore.clanDir + "/machines/eve/facts/zerotier-network-id"));
  services.zerotierone.joinNetworks = [ "7c31a21e86f9a75c" ];

  services.zerotierone.localConf.settings.physical = {
    "10.243.0.0/16".blacklist = true;
    "10.244.0.0/16".blacklist = true;
    "10.250.0.0/16".blacklist = true;
    "42::/16".blacklist = true;
  };
}
