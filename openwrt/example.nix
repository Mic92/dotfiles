{
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
      vpn = {
        _type = "interface";
        ifname = "tun0";
        proto = "static";
        ip6addr = "42:0:3c46:3f4b:cbe8:e276:a769:2a99/12";
      };
      lan = {
        _type = "interface";
        device = "br-lan";
        proto = "dhcp";
      };
      # When overriding `network.device`, don't forget to re-add `br-lan`, or you will log yourself out!
      device = [
        {
          _type = "device";
          name = "br-lan";
          type = "bridge";
          # list options are also supported
          ports = [
            "lan1"
            "lan2"
            "lan3"
            "lan4"
            "wan"
          ];
        }
      ];
    };
    wireless = {
      radio0 = {
        _type = "wifi-device";
        type = "mac80211";
        path = "platform/18000000.wmac";
        channel = "auto";
        band = "2g";
        htmode = "HT20";
        country = "DE";
        cell_density = "0";
      };
      radio1 = {
        _type = "wifi-device";
        type = "mac80211";
        path = "1a143000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0";
        channel = "auto";
        band = "5g";
        htmode = "HE160";
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

        myddns_ipv4 = common // {
          use_ipv6 = "0";
          ip_network = "wan";
          interface = "wan";
        };

        myddns_ipv6 = common // {
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
  };
  uci.secrets = {
    sops.files = [
      # Checkout https://github.com/mozilla/sops#encrypting-using-age
      # to learn how to create encrypted sops files.
      ./secrets.yml
    ];
  };
}
