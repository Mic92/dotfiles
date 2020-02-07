{ pkgs, config, lib, ... }: let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
in {
  imports = [
    ./whoami.nix
  ];
  services.knot = {
    enable = true;
    checkConfig = false;
    extraConfig = ''
      server:
        listen: ${ip4}@53
        listen: ${ip6}@53

      include: /var/lib/knot/knot-he-key.conf

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
          storage: /var/lib/knot/signed
          semantic-checks: on
          dnssec-signing: on
          dnssec-policy: rsa2k
          notify: [ he_ip4, he_ip6 ]
          acl: [ he_acl ]
          zonefile-load: difference

      zone:
        #- domain: thalheim.io
        #  file: "${./thalheim.io.zone}"
        #  template: master
        - domain: higgsboson.tk
          file: "${./higgsboson.tk.zone}"
          template: master
    '';
  };
  systemd.services.knot = {
    serviceConfig.PermissionsStartOnly = true;
    preStart = ''
      install -m700 --owner $USER /var/src/secrets/knot-he-key.conf /var/lib/knot/knot-he-key.conf
    '';
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
