{ pkgs, config, lib, ... }:
let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
  acmeChallenge = domain: pkgs.writeText "_acme-challenge.${domain}.zone" ''
    @ 3600 IN SOA _acme-challenge.${domain}. root.thalheim.io. 2021013110 7200 3600 86400 3600

    $TTL 600

    @ IN NS ns1.thalheim.io.
  '';
in
{
  imports = [
    ./whoami.nix
  ];

  sops.secrets."knot-he-key.conf".owner = "knot";
  sops.secrets."knot-acme-key.conf".owner = "knot";

  services.knot = {
    enable = true;
    keyFiles = [
      config.sops.secrets."knot-he-key.conf".path
      config.sops.secrets."knot-acme-key.conf".path
    ];
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

        - id: acme_acl
          key: acme
          action: update

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

        - id: acme
          semantic-checks: on
          dnssec-signing: on
          dnssec-policy: rsa2k
          acl: [ acme_acl ]
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
        - domain: r
          file: "${pkgs.retiolum}/zones/r.zone"
          template: retiolum
        - domain: w
          file: "${pkgs.retiolum}/zones/w.zone"
          template: retiolum
        - domain: i
          file: "${pkgs.retiolum}/zones/i.zone"
          template: retiolum
        - domain: _acme-challenge.thalheim.io
          file: "${acmeChallenge "thalheim.io"}"
          template: acme
        - domain: _acme-challenge.anon.thalheim.io
          file: "${acmeChallenge "anon.thalheim.io"}"
          template: acme
        - domain: _acme-challenge.dns.thalheim.io
          file: "${acmeChallenge "dns.thalheim.io"}"
          template: acme
        - domain: _acme-challenge.imap.thalheim.io
          file: "${acmeChallenge "imap.thalheim.io"}"
          template: acme
        - domain: _acme-challenge.mail.thalheim.io
          file: "${acmeChallenge "mail.thalheim.io"}"
          template: acme
        - domain: _acme-challenge.influxdb.thalheim.io
          file: "${acmeChallenge "influxdb.thalheim.io"}"
          template: acme
        - domain: _acme-challenge.lekwati.com
          file: "${acmeChallenge "lekwati.com"}"
          template: acme
        - domain: _acme-challenge.devkid.net
          file: "${acmeChallenge "devkid.net"}"
          template: acme
        - domain: _acme-challenge.imap.devkid.net
          file: "${acmeChallenge "imap.devkid.net"}"
          template: acme
    '';
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
