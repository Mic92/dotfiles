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
          name = "wan.7";
          type = "8021q";
          ifname = "wan";
          vid = "7";
        }
      ];
    };
    wireless = {
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
