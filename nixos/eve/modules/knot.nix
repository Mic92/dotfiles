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
  services.knot.enable = true;
  services.knot.extraConfig = ''
    server:
      listen: ${ip4}@53
      listen: ${ip6}@53
    zone:
    - domain: ip.thalheim.io
      file: "${whoamiZone { domain = "ip.thalheim.io"; ipv4 = true; ipv6 = true; }}"
      module: mod-whoami

    zone:
    - domain: ip4.thalheim.io
      file: "${whoamiZone { domain = "ip4.thalheim.io"; ipv4 = true; ipv6 = false; }}"
      module: mod-whoami

    zone:
    - domain: ip6.thalheim.io
      file: "${whoamiZone { domain = "ip6.thalheim.io"; ipv4 = false; ipv6 = true; }}"
      module: mod-whoami
  '';
}
