{
  uci.settings = {
    network = {
      wan = {
        _type = "interface";
        proto = "pppoe";
        username = "@pppoe_username@";
        password = "@pppoe_password@";
        ipv6 = "auto";
        device = "wan.7";
      };
      device = [
        {
          _type = "device";
          name = "br-lan";
          type = "bridge";
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
    dropbear.dropbear = [
      {
        _type = "dropbear";
        PasswordAuth = "off";
        Port = "22";
        Interface = "lan";
      }
    ];
  };
  uci.secrets = {
    sops.files = [
      ./secrets.yml
    ];
  };
}
