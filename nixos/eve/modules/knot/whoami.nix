{ pkgs, config, lib, ... }: let
  serial = "2020012901";
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
      @ NS ns1.thalheim.io.
    '';
in {
  services.knot = {
    extraConfig = ''
      zone:
      - domain: whoami.thalheim.io
        file: "${whoamiZone { domain = "whoami.thalheim.io"; ipv4 = true; ipv6 = true; }}"
        module: mod-whoami
      - domain: whoami4.thalheim.io
        file: "${whoamiZone { domain = "whoami4.thalheim.io"; ipv4 = true; ipv6 = false; }}"
        module: mod-whoami
      - domain: whoami6.thalheim.io
        file: "${whoamiZone { domain = "whoami6.thalheim.io"; ipv4 = false; ipv6 = true; }}"
        module: mod-whoami
    '';
  };
}
