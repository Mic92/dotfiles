{
  pkgs,
  config,
  lib,
  ...
}: let
  serial = "2020012901";
  whoamiZone = {
    domain,
    ipv4 ? false,
    ipv6 ? false,
  }:
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
      ${
        lib.optionalString ipv4 ''
          ns1 A ${config.networking.eve.ipv4.address}
        ''
      }
      ${
        lib.optionalString ipv6 ''
          ns1 AAAA ${lib.head config.networking.eve.ipv6.addresses}
        ''
      }
    '';
in {
  services.knot = {
    extraConfig = ''
      zone:
      - domain: whoami4.thalheim.io
        file: "${
        whoamiZone {
          domain = "whoami4.thalheim.io";
          ipv4 = true;
        }
      }"
        module: mod-whoami
      - domain: whoami6.thalheim.io
        file: "${
        whoamiZone {
          domain = "whoami6.thalheim.io";
          ipv6 = true;
        }
      }"
        module: mod-whoami
    '';
  };
}
