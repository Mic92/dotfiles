{ pkgs, config, lib, ... }: let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
in {
  imports = [
    ./whoami.nix
  ];

  krops.secrets."knot-he-key.conf".owner = "knot";
  users.users.knot.extraGroups = [ "keys" ];

  services.knot = {
    enable = true;
    keyFiles = [ config.krops.secrets."knot-he-key.conf".path ];
    extraConfig = ''
      server:
        listen: ${ip4}@53
        listen: ${ip6}@53

      remote:
        - id: he_ip4
          address: 216.218.130.2
        # does not accept NOTIFY yet
        #- id: he_ip6
        #  address: 2001:470:100::2

      acl:
        - id: he_acl
          key: he1
          action: transfer

      mod-rrl:
        - id: default
          rate-limit: 200   # Allow 200 resp/s for each flow
          slip: 2           # Every other response slips

      policy:
        - id: rsa2k
          algorithm: RSASHA256
          ksk-size: 4096
          zsk-size: 2048

      template:
        - id: default
          semantic-checks: on
          global-module: mod-rrl/default

        - id: master
          semantic-checks: on
          dnssec-signing: on
          dnssec-policy: rsa2k
          notify: [ he_ip4 ]
          acl: [ he_acl ]
          zonefile-sync: -1
          zonefile-load: difference
          journal-content: changes

      zone:
        - domain: thalheim.io
          file: "${./thalheim.io.zone}"
          template: master
        - domain: higgsboson.tk
          file: "${./higgsboson.tk.zone}"
          template: master
        - domain: lekwati.com
          file: "${./lekwati.com.zone}"
          template: master
    '';
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];

  services.icinga2.extraConfig = ''
    template Service "eve-dns4-service" {
      import "eve-service"
      check_command = "dig"
      vars.dig_server = host.address
    }
    template Service "eve-dns6-service" {
      import "eve-service"
      check_command = "dig"
      vars.dig_server = host.address6
      vars.dig_ipv6 = true
      vars.dig_record_type = "AAAA"
    }

    apply Service "DNS thalheim.io v4 (eve)" {
      import "eve-dns4-service"
      vars.dig_lookup = "thalheim.io"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "DNS thalheim.io v6 (eve)" {
      import "eve-dns6-service"
      vars.dig_lookup = "thalheim.io"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "DNS higgsboson.tk v4 (eve)" {
      import "eve-dns4-service"
      vars.dig_lookup = "higgsboson.tk"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "DNS higgsboson.tk v6 (eve)" {
      import "eve-dns6-service"
      vars.dig_lookup = "higgsboson.tk"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "DNS lekwati.com v4 (eve)" {
      import "eve-dns4-service"
      vars.dig_lookup = "lekwati.com"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "DNS lekwati.com v6 (eve)" {
      import "eve-dns6-service"
      vars.dig_lookup = "lekwati.com"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
