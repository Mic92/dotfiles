{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  network = (import ./network.nix) {inherit lib;};
  inherit (network) containers lxcContainers;
in {
  networking = {
    dhcpcd.enable = false;
    # use nftables instead
    firewall.enable = false;
    nameservers = with containers.dns; [
      ipv4 ipv6 ula
    ];
  };

  systemd.network = with network; {
    enable = true;
    netdevs = {
      ${network.bridge}.netdevConfig = {
        Name = network.bridge;
        Kind = "bridge";
      };
    };
    networks = {
      ${network.wan}.extraConfig = ''
        [Match]
        Name = ${network.wan}

        [Network]
        DHCP = ipv4
        IPv6ProxyNDP = yes
        ${lib.concatMapStrings (container: ''
          IPv6ProxyNDPAddress = ${container.ipv6}
        '') (attrValues lxcContainers)}
        ${lib.concatMapStrings (container: ''
          IPv6ProxyNDPAddress = ${container.ipv6}
        '') (attrValues config.eve.containers)}
        Address = ${network.ipv6}/128
        Address = 2a03:4000:13:31e:1::10/128
        Gateway = fe80::1
        IPv6AcceptRA = no
        IPForward = yes

        [DHCP]
        UseDNS = no
      '';
      ${bridge}.extraConfig = ''
        [Match]
        Name = ${bridge}

        [Network]
        Address = ${containers.bridge.ipv4}/26
        Address = ${containers.bridge.ipv6}/80
        Address = ${containers.bridge.ula}/80
        Address = fe80::1/64
        Address = 2a03:4000:13:31e:1::10/128

        IPForward = yes
        Domains = ${network.zone.dn42-domain} ~dn42

        [Route]
        Gateway = ${containers.dn42.ip}
        Destination = 172.16.0.0/12

        [Route]
        Gateway = ${containers.dn42.ip}
        Destination = 10.0.0.0/8

        [Route]
        Gateway = ${containers.dn42.ula}
        Destination = fc00::/7

        [Route]
        Gateway = ${containers.dn42.ip6}
        Destination = ${network.ipv6}/64
      '';
      lxc.extraConfig = ''
        [Match]
        Name = lxc_*

        [Network]
        LinkLocalAddressing = no
        Bridge = br0
      '';
    };
  };
}
