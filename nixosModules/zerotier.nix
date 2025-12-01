{ config, lib, ... }:
{
  options = {
    services.zerotierone = {
      blockRfc1918Addresses = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          If true, blocks RFC1918 addresses using the firewall to stop zerotier from connecting to it.
          Some providers such as Hetzner will sent out abuse reports if you connect to these addresses.
        '';
      };
    };
  };
  config = {
    systemd.services.zerotierone.serviceConfig.IPAddressDeny =
      lib.mkIf config.services.zerotierone.blockRfc1918Addresses
        [
          "10.0.0.0/8"
          "172.16.0.0/12"
          "192.168.0.0/16"
        ];
    services.zerotierone.joinNetworks = [
      "ccc5da5295c853d4"
      "b15644912e61dbe0"
    ];

    services.zerotierone.localConf.settings = {
      interfacePrefixBlacklist = [
        "tinc"
        "wiregrill"
        "tailscale"
      ];
    };
  };
}
