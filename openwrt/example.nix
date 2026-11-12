{
  pkgs,
  lib,
  retiolumHosts,
  kartei,
  ...
}:
let
  # OpenSSL finds CAs in /etc/ssl/certs by subject hash
  krebsIntermediateHash = "8cbb0531";
  krebsIntermediate =
    pkgs.runCommand "krebs-intermediate-ca.crt" { nativeBuildInputs = [ pkgs.openssl ]; }
      ''
        hash=$(openssl x509 -noout -subject_hash -in ${kartei}/modules/ca/intermediate-ca.crt)
        if [ "$hash" != ${krebsIntermediateHash} ]; then
          echo "krebs intermediate CA changed, set krebsIntermediateHash to $hash" >&2
          exit 1
        fi
        cp ${kartei}/modules/ca/intermediate-ca.crt $out
      '';
in
{
  openwrt.packages = [
    "luci"
    "tcpdump"
    "tinc"
    "rsync"
    "ddns-scripts-nsupdate"
    "iperf3"
    "luci-mod-rpc"
    "prometheus-node-exporter-lua"
    "prometheus-node-exporter-lua-openwrt"
    "acme-acmesh"
  ];

  openwrt.rootPasswordSecret = "root_password";

  openwrt.files = {
    "/etc/dropbear/authorized_keys" = {
      text = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDV5xoYdIHYO7T2FtbIcV8LmiPmJc7JcxElSaGD9tzf joerg@turingmachine rauter";
      mode = "0600";
    };
    "/etc/dropbear/dropbear_ed25519_host_key" = {
      secret = "dropbear_ed25519_host_key";
      base64 = true;
    };
    "/etc/dropbear/dropbear_rsa_host_key" = {
      secret = "dropbear_rsa_host_key";
      base64 = true;
    };
    # for https://ca.r, constrained to .r and .w unlike the root
    "/etc/ssl/certs/${krebsIntermediateHash}.0".source = krebsIntermediate;
    # uhttpd self-signs at its configured path if the file is missing, so
    # copy over the default instead of pointing it at /etc/ssl/acme
    "/etc/hotplug.d/acme/50-uhttpd".text = ''
      if [ "$ACTION" = issued ] || [ "$ACTION" = renewed ]; then
        # leaf only: mbedtls cannot parse the name constrained intermediate
        cp /etc/ssl/acme/rauter.r.crt /etc/uhttpd.crt
        cp /etc/ssl/acme/rauter.r.key /etc/uhttpd.key
        /etc/init.d/uhttpd restart
      fi
    '';
    # must match mic92/hosts/rauter in krebs/kartei
    "/etc/tinc/retiolum/rsa_key.priv".secret = "tinc_rsa_key";
    "/etc/tinc/retiolum/ed25519_key.priv".secret = "tinc_ed25519_key";
    "/etc/tinc/retiolum/hosts".source = pkgs.linkFarm "retiolum-tinc-hosts" (
      lib.mapAttrsToList (name: text: {
        inherit name;
        path = pkgs.writeText name text;
      }) retiolumHosts.tincHosts
    );
    # dnsmasq serves /etc/hosts, so LAN clients resolve .r names too
    "/etc/hosts".text = ''
      127.0.0.1 localhost
      ::1 localhost ip6-localhost ip6-loopback
      ff02::1 ip6-allnodes
      ff02::2 ip6-allrouters
      ${retiolumHosts.extraHosts.v6only}
    '';
  };

  # none of these pick up changed files on reload
  openwrt.restartServices = [
    "dropbear"
    "tinc"
    "dnsmasq"
  ];

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
        # sysupgrade treats a missing value as 1.0 and rejects 25.12 images
        compat_version = "2.0";
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
    # named sections are recreated, so repeat luci-app-attendedsysupgrade's defaults
    attendedsysupgrade.client = {
      _type = "client";
      upgrade_packages = "1";
      auto_search = "0";
      advanced_mode = "0";
      login_check_for_upgrades = "1";
    };

    tinc.retiolum = {
      _type = "tinc-net";
      enabled = 1;
      Name = "rauter";
      Interface = "tun0";
      ConnectTo = [
        "eva"
        "eve"
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

    acme = {
      acme = [
        {
          _type = "acme";
          account_email = "joerg@thalheim.io";
        }
      ];
      rauter = {
        _type = "cert";
        enabled = "1";
        domains = [ "rauter.r" ];
        acme_server = "https://ca.r/acme/acme/directory";
        validation_method = "webroot";
        # opens port 80 in the firewall only while a validation runs
        listen_port = "80";
      };
    };

    # retiolum is shared with other people: only allow what listens on vpn
    firewall = {
      vpn_zone = {
        _type = "zone";
        name = "vpn";
        network = [ "vpn" ];
        input = "REJECT";
        output = "ACCEPT";
        forward = "REJECT";
      };
      vpn_ping = {
        _type = "rule";
        name = "Allow-VPN-Ping";
        src = "vpn";
        family = "ipv6";
        proto = "icmp";
        icmp_type = [ "echo-request" ];
        target = "ACCEPT";
      };
      vpn_services = {
        _type = "rule";
        name = "Allow-VPN-Services";
        src = "vpn";
        family = "ipv6";
        proto = "tcp";
        dest_port = [
          "22"
          "443"
          "9273"
        ];
        target = "ACCEPT";
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
