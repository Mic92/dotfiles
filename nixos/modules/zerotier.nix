{ pkgs, ... }:
{
  clan.networking.zerotier.networkId = builtins.readFile ../../machines/eve/facts/zerotier-network-id;

  systemd.tmpfiles.rules = [
    "L+ /var/lib/zerotier-one/local.conf - - - - ${pkgs.writeText "local.conf" (builtins.toJSON {
      physical = {
        "10.243.0.0/16".blacklist = true;
        "10.244.0.0/16".blacklist = true;
        "10.250.0.0/16".blacklist = true;
        "42::/16".blacklist = true;
      };
    })}"
  ];

  networking.networkmanager.unmanaged = [ "interface-name:zt*" ];
}
