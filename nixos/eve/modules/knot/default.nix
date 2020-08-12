{ pkgs, config, lib, ... }: let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
in {
  imports = [
    ./whoami.nix
  ];

  sops.secrets."knot-he-key.conf".owner = "knot";
  users.users.knot.extraGroups = [ "keys" ];

  services.knot = {
    enable = true;
    keyFiles = [ config.sops.secrets."knot-he-key.conf".path ];
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

        - id: retiolum
          semantic-checks: on
          dnssec-signing: on
          dnssec-policy: rsa2k
          zonefile-sync: -1
          zonefile-load: difference
          journal-content: changes

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
        - domain: lekwati.com
          file: "${./lekwati.com.zone}"
          template: master
        ${lib.concatMapStringsSep "\n" (domain: ''
        - domain: ${domain}
          file: "${pkgs.retiolum}/zones/${domain}.zone"
          template: retiolum
        '') [ "w" "r" "i" "lan" "shack" "gg23" ]}
    '';
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
