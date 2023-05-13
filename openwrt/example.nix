{ lib, ... }: {
  uci.settings = {
    # The block below will translate to the following uci settings:
    # root@OpenWrt:~# uci show dropbear
    #dropbear.@dropbear[0]=dropbear
    #dropbear.@dropbear[0].Interface='lan'
    #dropbear.@dropbear[0].PasswordAuth='off'
    #dropbear.@dropbear[0].Port='22'
    dropbear.dropbear = [
      {
        # each section needs a type, denoted by `_type`
        _type = "dropbear";
        # those are normal config options
        PasswordAuth = "off";
        Port = "22";
        GatewayPorts = "1";
        Interface = "lan";
      }
      {
        # each section needs a type, denoted by `_type`
        _type = "dropbear";
        # those are normal config options
        PasswordAuth = "off";
        Port = "22";
        GatewayPorts = "1";
        Interface = "vpn";
      }
    ];

    # Use `uci show <config>` to translate your existing configuration to the uci equivalent.
    # Note that once you define a section i.e. network.wan, than all existing
    # values of this section are unset before applying the new values

    system.system = [
      {
        _type = "system";
        hostname = "rauter";
      }
    ];

    network = {
      wan = {
        _type = "interface";
        proto = "pppoe";
        # values with @key@ are replaced by secrets loaded via the sops files at the end of this module
        username = "@pppoe_username@";
        password = "@pppoe_password@";
        ipv6 = "auto";
        device = "wan.7";
      };
      vpn = {
        _type = "interface";
        ifname = "tun0";
        proto = "static";
        ip6addr = "42:0:3c46:3f4b:cbe8:e276:a769:2a99/12";
      };
      # When overriding `network.device`, don't forget to re-add `br-lan`, or you will log yourself out!
      device = [
        {
          _type = "device";
          name = "br-lan";
          type = "bridge";
          # list options are also supported
          ports = [ "lan1" "lan2" "lan3" "lan4" ];
        }
        {
          _type = "device";
          name = "wan.7";
          type = "8021q";
          ifname = "wan";
          vid = "7";
        }
      ];
    };
    wireless = {
      radio0 = {
        _type = "wifi-device";
        type = "mac80211";
        path = "platform/18000000.wmac";
        channel = "1";
        band = "2g";
        htmode = "HT20";
        country = "DE";
        cell_density = "0";
      };
      radio1 = {
        _type = "wifi-device";
        type = "mac80211";
        path = "1a143000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0";
        channel = "36";
        band = "5g";
        htmode = "HE80";
        country = "DE";
        cell_density = "0";
      };
      default_radio0 = {
        _type = "wifi-iface";
        device = "radio0";
        network = "lan";
        mode = "ap";
        ssid = "gchq-2.4";
        encryption = "sae-mixed";
        key = "@wifi_password@";
      };
      default_radio1 = {
        _type = "wifi-iface";
        device = "radio1";
        network = "lan";
        mode = "ap";
        ssid = "gchq-5";
        encryption = "sae-mixed";
        key = "@wifi_password@";
      };
    };
    tinc.retiolum = {
      _type = "tinc-net";
      enabled = 1;
      Name = "rauter";
      Interface = "tun0";
      ConnectTo = [
        "eva"
        "eve"
        "ni"
        "prism"
      ];
    };

    dhcp.host = lib.imap0
      (id: host:
        rec {
          _type = "host";
          dns = "1";
          ip = "192.168.1.${hostid}";
          hostid = toString (id + 2);
        }
        // host) [
      {
        name = "rock";
        mac = "4a:4e:25:af:9e:0f";
      }
      {
        name = "turingmachine";
        mac = "c4:03:a8:86:a2:95";
      }
      {
        name = "bernie";
        mac = "14:18:C3:BB:6F:07";
      }
      {
        name = "oneplus-nord";
        mac = "D0:49:7C:36:5B:80";
      }
      {
        name = "livingroom";
        mac = "7C:DF:A1:B5:11:B0";
      }
    ];

    ddns =
      let
        common = {
          _type = "service";
          enabled = "1";
          service_name = "bind-nsupdate";
          lookup_host = "rauter.thalheim.io";
          domain = "rauter.thalheim.io.";
          ip_source = "network";
          dns_server = "ns1.thalheim.io";
          use_syslog = "2";
          username = "hmac-sha256:rauter";
          password = "@tsig_key@";
          check_unit = "minutes";
          force_unit = "minutes";
          retry_unit = "seconds";
        };
      in
      {
        global = {
          _type = "ddns";
          ddns_dateformat = "%F %R";
          ddns_loglines = "250";
          ddns_rundir = "/var/run/ddns";
          ddns_logdir = "/var/log/ddns";
          upd_privateip = "0";
        };

        myddns_ipv4 =
          common
          // {
            use_ipv6 = "0";
            ip_network = "wan";
            interface = "wan";
          };

        myddns_ipv6 =
          common
          // {
            use_ipv6 = "1";
            ip_network = "wan_6";
            interface = "wan_6";
          };
      };

    prometheus-node-exporter-lua.main = {
      _type = "prometheus-node-exporter-lua";
      listen_interface = "vpn";
      listen_port = "9273";
    };

    firewall.redirect = [{
      _type = "redirect";
      enabled = "1";
      name = "Forward-ESPHome";
      src = "wan";
      dest = "lan";
      dest_ip = "192.168.1.6"; # livingroom
      proto = "tcp";
      src_dport = 6053;
      dest_port = 6053;
    }];

    firewall.rule = [
      # Not needed for pppoe
      #{
      #  _type = "rule";
      #  name = "Allow-DHCP-Renew";
      #  proto = "udp";
      #  src = "wan";
      #  dest_port = 68;
      #  family = "ipv4";
      #  target = "ACCEPT";
      #}
      # only needed for ISP iptv
      #{
      #  _type = "rule";
      #  name = "Allow-IGMP";
      #  proto = "icmp";
      #  src = "wan";
      #  icmp_type = "echo-request";
      #  family = "ipv4";
      #  target = "ACCEPT";
      #}
      {
        _type = "rule";
        name = "Allow-Ping";
        proto = "icmp";
        src = "wan";
        icmp_type = "echo-request";
        family = "ipv4";
        target = "ACCEPT";
      }
      {
        _type = "rule";
        name = "Allow-DHCPv6";
        src = "wan";
        proto = "udp";
        src_ip = "fc00::/6";
        dest_ip = "fc00::/6";
        dest_port = 546;
        family = "ipv6";
        target = "ACCEPT";
      }
      # "The WAN port should at least respond to MLD queries as otherwise a
      # snooping bridge/switch might drop traffic."
      {
        _type = "rule";
        name = "Allow-MLD";
        src = "wan";
        proto = "icmp";
        src_ip = "fe80::/10";
        icmp_type = [ "130/0" "131/0" "132/0" "143/0" ];
        family = "ipv6";
        target = "ACCEPT";
      }
      {
        _type = "rule";
        name = "Allow-ICMPv6-Input";
        src = "wan";
        proto = "icmp";
        src_ip = "fe80::/10";
        icmp_type = [
          "echo-request"
          "echo-reply"
          "destination-unreachable"
          "packet-too-big"
          "time-exceeded"
          "bad-header"
          "unknown-header-type"
          "router-solicitation"
          "neighbour-solicitation"
          "router-advertisement"
          "neighbour-advertisement"
        ];
        limit = "1000/sec";
        family = "ipv6";
        target = "ACCEPT";
      }
      {
        _type = "rule";
        name = "Allow-ICMPv6-Forward";
        src = "wan";
        dest = "*";
        icmp_type = [
          "echo-request"
          "echo-reply"
          "destination-unreachable"
          "packet-too-big"
          "time-exceeded"
          "bad-header"
          "unknown-header-type"
        ];
        limit = "1000/sec";
        family = "ipv6";
        target = "ACCEPT";
      }
      # no need for ipsec
      #{
      #  _type = "rule";
      #  name = "Allow-IPSec-ESP";
      #  src = "wan";
      #  dest = "lan";
      #  proto = "esp";
      #  target = "ACCEPT";
      #}
      #{
      #  _type = "rule";
      #  name = "Allow-ISAKMP";
      #  src = "wan";
      #  dest = "lan";
      #  dest_port = "500";
      #  proto = "udp";
      #  target = "ACCEPT";
      #}
      # own rules follow here
      {
        _type = "rule";
        name = "Allow-Tinc";
        src = "wan";
        dest = "lan";
        dest_port = "655";
        family = "ipv6";
        target = "ACCEPT";
      }
    ];
  };
  uci.secrets = {
    sops.files = [
      # Checkout https://github.com/mozilla/sops#encrypting-using-age
      # to learn how to create encrypted sops files.
      ./secrets.yml
    ];
  };
}
