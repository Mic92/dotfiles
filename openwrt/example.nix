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

    system.system = [{
      _type = "system";
      hostname = "rauter";
    }];

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
          ports = ["lan1" "lan2" "lan3" "lan4"];
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
  };
  uci.secrets = {
    sops.files = [
      # Checkout https://github.com/mozilla/sops#encrypting-using-age
      # to learn how to create encrypted sops files.
      ./secrets.yml
    ];
  };
}
