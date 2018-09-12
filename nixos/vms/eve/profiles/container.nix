{ lib, config, pkgs, ... }:
with lib;
{

  options = {
    eve = mkOption {
      description = ''
        Custom extensions of config.containers
      '';
      type = types.submodule (import ../modules/container-options.nix);
    };
  };
  config = {

    systemd.network.enable = true;
    services.resolved.enable = false;
    networking.firewall.enable = false;
    systemd.network.networks = {
      public.extraConfig = ''
        [Match]
        Name = host0

        [Network]
        Address = ${config.eve.natIpv4}/10
        Address = ${config.eve.ipv6}/128
        Gateway = 100.64.0.2

        [Route]
        Destination = 2000::/3
        Gateway = fe80::1
      '';
      internal.extraConfig = ''
        [Match]
        Name = internal

        [Network]
        Address = ${config.eve.internalIpv4}/26
        Address = ${config.eve.internalIpv6}/64
        LinkLocalAddressing = no
      '';
    };
  };
}
