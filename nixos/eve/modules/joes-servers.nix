{ config, lib, pkgs, ... }:
{
  # allow networkd DHCP requests and dns
  networking.firewall.interfaces."br-joe".allowedUDPPorts = [
    53
    67
  ];

  systemd.network.networks."50-lo".extraConfig = ''
    [Match]
    Name = lo

    [Network]
    Address = 192.168.53.53/32
  '';

  systemd.network.netdevs.internal.netdevConfig = {
    Name = "br-joe";
    Kind = "bridge";
  };

  systemd.network.networks."50-joe".extraConfig = ''
    [Match]
    Name = br-joe

    [Network]
    Address = 192.168.21.254/24
    Address = 2a01:4f8:10b:49f:1::/80
    LinkLocalAddressing = yes
    DHCPServer = yes
    IPMasquerade = yes
    LLDP = yes
    EmitLLDP = customer-bridge
    IPv6SendRA = yes

    [DHCPServer]
    EmitDNS = yes
    DNS = 192.168.53.53
  '';

  networking.nat = {
    enable = true;
    enableIPv6 = true;
    externalInterface = "eth0";
    internalInterfaces = [
      "br-joe"
    ];
    forwardPorts = [
      # joe01
      {
        sourcePort = 2201;
        loopbackIPs = [
          "192.168.21.254"
        ];
        destination = "192.168.21.1:22";
      }
      {
        sourcePort = 2201;
        destination = "[2a01:4f8:10b:49f:1::1]:22";
      }
    ];
  };

  systemd.nspawn.joe01 = {
    enable = true;
    execConfig = {
      Capability = "all";
      PrivateUsers = "no";
    };
    networkConfig = {
      Port = "tcp:2222:22";
      Bridge = "br-joe";
    };
  };
  # email: j03@c3d2.de
}
