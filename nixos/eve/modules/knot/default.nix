{ pkgs, config, lib, ... }: let

  serial = "2020012901";
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
  whoamiZone = { domain, ipv4 ? false, ipv6 ? false }:
    pkgs.writeText "${domain}.zone" ''
      $TTL 1
      @ SOA (
        ${domain}. ; MNAME
        hostmaster.thalheim.io. ; RNAME
        ${serial} ; SERIAL
        86400 ; REFRESH
        86400 ; RETRY
        86400 ; EXPIRE
        1 ; MINIMUM
      )
      $TTL 86400
      @ NS ns1
      ${lib.optionalString ipv4 ''
        ns1 A ${ip4}
      ''}
      ${lib.optionalString ipv6 ''
        ns1 AAAA ${ip6}
      ''}
    '';
in {
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
          address: 216.218.133.2
        - id: he_ip6
          address: 2001:470:600::2

      acl:
        - id: he_acl
          key: he_key
          action: transfer

      zone:
        - domain: thalheim.io
          file: "${./thalheim.io.zone}"
          notify: [he_ip4, he_ip6]
          acl: [he_acl]
        - domain: higgsboson.tk
          file: "${./higgsboson.tk.zone}"
          notify: [he_ip4, he_ip6]
          acl: [he_acl]
        - domain: ip.thalheim.io
          file: "${whoamiZone { domain = "ip.thalheim.io"; ipv4 = true; ipv6 = true; }}"
          module: mod-whoami
        - domain: ip4.thalheim.io
          file: "${whoamiZone { domain = "ip4.thalheim.io"; ipv4 = true; ipv6 = false; }}"
          module: mod-whoami
        - domain: ip6.thalheim.io
          file: "${whoamiZone { domain = "ip6.thalheim.io"; ipv4 = false; ipv6 = true; }}"
          module: mod-whoami
    '';
  };
  systemd.services.knot = {
    serviceConfig.PermissionsStartOnly = true;
    preStart = ''
      install -m700 --owner $USER /var/src/secrets/knot-he-key.conf /var/lib/knot/knot-he-key.conf
    '';
  };
}
