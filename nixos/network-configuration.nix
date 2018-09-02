{ config, lib, pkgs, ...}:
with lib;

let
  internetSharing = {
    enable = false;
    hotspot = false;
  };
  network = (import ./network.nix);
in {

  imports = [
    ./vms/modules/retiolum.nix
    ./vms/modules/networkd.nix
  ];

  services = {
    dnsmasq.extraConfig = ''
      #server=127.0.0.1#5353
      #server=/dn42/172.23.75.6

      no-resolv
      cache-size=1000
      min-cache-ttl=3600
      bind-dynamic
      all-servers

      dnssec
      trust-anchor=.,19036,8,2,49AAC11D7B6F6446702E54A1607371607A1A41855200FD2CE1CDDE32F24E8FB5

      address=/blog/127.0.0.1
      address=/blog/::1
      rebind-domain-ok=/onion/
      server=/.onion/127.0.0.1#9053
      port=53
      #log-queries
      ${if internetSharing.hotspot then
      ''
        interface=wlp3s0
        dhcp-option=wlp3s0,1,255.255.255.0  # subnet
        dhcp-option=wlp3s0,3,192.168.44.254 # router
        dhcp-option=wlp3s0,6,192.168.44.254 # dns
        dhcp-range=wlp3s0,192.168.44.0,192.168.44.253,12h
      '' else if internetSharing.enable then ''
        interface=enp0s25
        dhcp-option=enp0s25,1,255.255.255.0  # subnet
        dhcp-option=enp0s25,3,192.168.43.254 # router
        dhcp-option=enp0s25,6,192.168.43.254 # dns
        dhcp-range=enp0s25,192.168.43.0,192.168.43.253,12h
      '' else ""}
    '';
    hostapd = {
      enable = internetSharing.hotspot;
      ssid = "cipherpunk";
      wpaPassphrase = "cipherpunk";
      interface = network.wlan_interface;
    };
  };

  services.kresd = let
    certificate = pkgs.fetchurl {
      url = "https://letsencrypt.org/certs/isrgrootx1.pem.txt";
      sha256 = "0zhd1ps7sz4w1x52xk3v7ng6d0rcyi7y7rcrplwkmilnq5hzjv1y";
    };
  in {
    enable = true;
    extraConfig = ''
      modules = {
        predict = {
           window = 15, -- 15 minutes sampling window
           period = 6*(60/15) -- track last 6 hours
        },
        -- activate later https://github.com/CZ-NIC/knot-resolver/commit/c766411a8868b57cb8a9f3c1536b32b5204ffcd3#r27794868
        --'serve_stale < cache',
      }
      policy.add(policy.all(policy.TLS_FORWARD({
        --{ '2620:fe::fe', hostname = 'dns.quad9.net', ca_file = '${certificate}' },
        --{ '9.9.9.9', hostname = 'dns.quad9.net', ca_file = '${certificate}' },
        { '9.9.9.9', hostname = 'dns.quad9.net', ca_file = '/etc/ssl/certs/ca-bundle.crt' },
        { '2620:fe::fe', hostname = 'dns.quad9.net', ca_file = '/etc/ssl/certs/ca-bundle.crt' },
      })))
    '';
  };

  networking = {
    networkmanager.enable = true;
    networkmanager.packages = [ pkgs.networkmanager-vpnc ];

    retiolum = {
      ipv4 = "10.243.29.168";
      ipv6 = "42:4992:6a6d:600::1";
    };

    defaultMailServer = {
      directDelivery = true;
      hostName = "mail.thalheim.io:587";
      root = "joerg@thalheim.io";
      authUser = "joerg@higgsboson.tk";
      authPassFile = "/etc/nixos/secrets/smtp-authpass";
      domain = "thalheim.io";
      useSTARTTLS = true;
    };
    nameservers = ["9.9.9.9"];

    firewall.enable = true;
    firewall.allowedTCPPorts = [ 3030 ];
    hostName = "turingmachine";
    #wireless.enable = !internetSharing.hotspot;
    wireless.iwd.enable = false;
    dhcpcd.enable = false;
  };

  systemd.network.netdevs = let
    wgTemplate = lport: name: endpoint: key: {
      netdevConfig = { Name = "wg-${name}"; Kind = "wireguard"; };
      extraConfig = ''
       [WireGuard]
       PrivateKey = ${lib.readFile ./secrets/wireguard-key}
       ListenPort = ${toString lport}
       FwMark = 42

       [WireGuardPeer]
       AllowedIPs = 0.0.0.0/1
       AllowedIPs = ::/0
       Endpoint = ${endpoint}
       PublicKey = ${key}
      '';
    };
  in {
    #wg-eve = wgTemplate 42421 "eve" "ipv4.dn42.higgsboson.tk:42422" "fxiGmHUK1aMa07cejTP3SHxYivIj3aXZwdvzTEXmYHM=";
    #wg-eve6 = wgTemplate 42422 "eve6" "ipv6.dn42.higgsboson.tk:42422" "fxiGmHUK1aMa07cejTP3SHxYivIj3aXZwdvzTEXmYHM=";
    #wg-rauter = wgTemplate 42423 "rauter" "rauter.he.thalheim.io:42422" "l6LjG1WuLNkEwd2047mw2GpgPUppM1VwP/LWMaOqJ0E=";
    #wg-matchbox = wgTemplate 42424 "matchbox" "matchbox.he.thalheim.io:42432" "6ExGu7MjeHoPbWj8/F3YNcdMHa7e3fXFFPkswAXv4T4=";

    #anboxbr0.netdevConfig = { Name = "anboxbr0"; Kind = "bridge"; };

    dummy0.netdevConfig = { Name = "dummy0"; Kind = "dummy"; };
  };
  systemd.network.networks = {
    ethernet.extraConfig = ''
      [Match]
      Name = enp*

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      ${optionalString internetSharing.enable ''
        IPForward=yes
        Address=192.168.43.254/24
      ''}
      LLDP=true
      IPv6AcceptRA=true
      IPv6Token=::521a:c5ff:fefe:65d8

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
    ethernet2.extraConfig = ''
      [Match]
      Name = eth*

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      ${optionalString internetSharing.enable ''
        IPForward=yes
        Address=192.168.43.254/24
      ''}
      LLDP=true
      IPv6AcceptRA=true
      IPv6Token=::521a:c5ff:fefe:65d8

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
    wlan.extraConfig = ''
      [Match]
      Name = wl*

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      ${optionalString internetSharing.enable ''
        IPForward=yes
        Address=192.168.44.254/24
      ''}
      IPForward=yes
      LLDP=true
      IPv6AcceptRA=true
      #VRF=physical

      [DHCP]
      UseHostname=false
      RouteMetric=1024
    '';
    dummy.extraConfig = ''
      [Match]
      Name=dummy0

      [Network]
      Address=${network.ip4_prefix}/32
      Address=${network.ula_prefix}::1/64

      [Address]
      Address=${network.ip6_prefix}::1/120
      PreferredLifetime=0
    '';
    wg-eve.extraConfig = ''
      [Match]
      Name=wg-eve

      [Network]
      DNS=172.23.75.6
      Domains=~dn42
      DNSSEC=allow-downgrade
      BindCarrier=enp*,wlp*
      Address=fe80::${network.router_id}/64

      [Address]
      Address=169.254.1.${network.router_id}/16
      PreferredLifetime=0
      Scope=link

      [Route]
      Destination=2000::/3
      Source=2a03:4000:13:31e:200::/120
      Gateway=fe80::1
      Metric=512
    '';
    wireguard.extraConfig = ''
      [Match]
      Name=wg-*

      [Network]
      BindCarrier=enp*,wlp*
      Address=fe80::${network.router_id}/64

      [Address]
      Address=169.254.1.${network.router_id}/16
      PreferredLifetime=0
      Scope=link
    '';
    tethering.extraConfig = ''
      [Match]
      Name=enp0s20u*

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=false
      IPForward=yes
      LLDP=true
      Address=192.168.42.254/24

      [DHCP]
      UseHostname=false
      RouteMetric=2048
    '';
    bluetooth.extraConfig = ''
      [Match]
      Name=bnep0

      [Network]
      DHCP=both
      LLMNR=true
      IPv4LL=true
      IPForward=yes
      LLDP=true
      Address=192.168.42.254/24

      [DHCP]
      UseHostname=false
      RouteMetric=4096
    '';
    "10usbnetwork".extraConfig = ''
      [Match]
      Driver=r8152

      [Network]
      DHCPServer=yes
      Address=192.168.41.254/24
    '';
    #anboxbr0.extraConfig = ''
    #  [Match]
    #  Name=anboxbr0

    #  [Network]
    #  Address=0.0.0.0/24
    #  DHCPServer=yes
    #  IPMasquerade=yes

    #  [DHCPServer]
    #  ##Manual provide DNS Server
    #  #DNS=8.8.8.8
    #'';
  };
}
