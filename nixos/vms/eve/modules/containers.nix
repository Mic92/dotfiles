{ lib, config, pkgs, ... }:

with lib;

let 
  cfg = config.eve.containers;
  nspawnContainers = filterAttrs (_: c: c.type == "nspawn") config.eve.containers;
in {
  options = {
    eve.containers = mkOption {
      description = ''
        Custom extensions of config.containers
      '';
      type = types.attrsOf (types.submodule (import ./container-options.nix));
    };
  };

  config = {
    containers = mapAttrs (name: container: {
      autoStart = false;
      privateNetwork = false;
      extraFlags = [
        "--private-network"
        "--network-veth-extra=vc-${name}:host0"
        "--network-veth-extra=br-${name}:internal"
      ];
      config = {...}: {
        imports = [ ../profiles/container.nix ];
        eve = container;
      };
    }) nspawnContainers;

    systemd.network.networks = (mapAttrs (name: container: {
      extraConfig = ''
        [Match]
        Name = vc-${name}

        [Network]
        Address = 100.64.0.2/32
        Address = fe80::1/64
        LinkLocalAddressing = no

        [Route]
        Destination = ${container.natIpv4}/32

        [Route]
        Destination = ${container.ipv6}/128
      '';
    }) nspawnContainers) // (mapAttrs' (name: container: nameValuePair "br-${name}" {
      extraConfig = ''
        [Match]
        Name = br-${name}

        [Network]
        Bridge = br0
        LinkLocalAddressing = no
      '';
    }) nspawnContainers);
  };
}
