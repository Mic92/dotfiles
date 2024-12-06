{
  pkgs,
  config,
  inputs,
  lib,
  ...
}:
let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = config.networking.eve.ipv6.address;
  dyndns =
    domain:
    pkgs.writeText "${domain}.zone" ''
      @ 3600 IN SOA ${domain}. ns1.thalheim.io. 2021013112 7200 3600 86400 3600

      $TTL 300

      @ IN NS ns1.thalheim.io.
    '';

  zoneWithAcme =
    name:
    pkgs.writeText "${name}.zone" ''
      ${builtins.readFile (./. + "/${name}.zone")}
      ${lib.concatMapStringsSep "\n" (name: "_acme-challenge.${name}. IN NS ns1.thalheim.io.") (
        builtins.filter (name: lib.strings.hasSuffix ".${name}" name) (
          builtins.attrNames config.security.acme.certs
        )
      )}
    '';
in
{
  #content of the secret
  #key:
  #- id: acme
  #  algorithm: hmac-sha256
  #  secret: 00000000000000000000000000000000000000000000
  #nix-shell -p knot-dns --run 'keymgr -t my_name hmac-sha256'
  sops.secrets."knot-keys.conf".owner = "knot";

  services.knot = {
    enable = true;
    keyFiles = [ config.sops.secrets."knot-keys.conf".path ];
    settings = {
      server = {
        listen = [
          "${ip4}@53"
          "${ip6}@53"
        ];
      };

      remote = [
        {
          id = "he_ip4";
          address = "216.218.130.2";
          # does not accept NOTIFY yet
          #- id: he_ip6
          #  address: 2001:470:100::2
        }
      ];

      # to generate TSIG key
      # for i in host; do keymgr -t $i; done
      acl = [
        {

          id = "he_acl";
          key = "he1";
          action = "transfer";
        }
        {
          id = "acme_acl";
          key = "acme";
          action = "update";
        }
        {
          id = "matchbox_acl";
          key = "matchbox";
          action = "update";
        }
        {
          id = "turingmachine_acl";
          key = "turingmachine";
          action = "update";
        }
        {
          id = "blob64_acl";
          key = "blob64";
          action = "update";
        }
        {
          id = "rauter_acl";
          key = "rauter";
          action = "update";
        }
        {
          id = "bernie_acl";
          key = "bernie";
          action = "update";
        }
      ];

      mod-rrl = [
        {
          id = "default";
          rate-limit = 200;
          slip = 2;
        }
      ];

      policy = [
        {
          id = "default";
          algorithm = "RSASHA256";
          ksk-size = 4096;
          zsk-size = 2048;
        }
      ];

      template = [
        {
          id = "default";
          semantic-checks = "on";
          global-module = "mod-rrl/default";
        }
        {
          id = "retiolum";
          semantic-checks = "on";
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
        {
          id = "master";
          semantic-checks = "on";
          notify = [ "he_ip4" ];
          acl = [ "he_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
        {
          id = "acme";
          semantic-checks = "on";
          acl = [ "acme_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
        {
          id = "dyndns";
          semantic-checks = "on";
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
        {
          id = "bernie";
          semantic-checks = "on";
          acl = [ "bernie_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
        {
          id = "matchbox";
          semantic-checks = "on";
          acl = [ "matchbox_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference-no-serial";
          serial-policy = "dateserial";
          journal-content = "all";
        }
      ];
      zone =
        [
          {
            domain = "thalheim.io";
            file = zoneWithAcme "thalheim.io";
            template = "master";
          }
          {
            domain = "lekwati.com";
            file = zoneWithAcme "lekwati.com";
            template = "master";
          }
          {
            domain = "tierheilpraxis-jessican.de";
            file = ./tierheilpraxis-jessican.de.zone;
            template = "master";
          }
          {
            domain = "r";
            file = "${inputs.retiolum}/zones/r.zone";
            template = "retiolum";
          }
          {
            domain = "w";
            file = "${inputs.retiolum}/zones/w.zone";
            template = "retiolum";
          }
          {
            domain = "matchbox.thalheim.io";
            file = dyndns "matchbox.thalheim.io";
            template = "dyndns";
            acl = [ "matchbox_acl" ];
          }
          {
            domain = "bernie.thalheim.io";
            file = dyndns "bernie.thalheim.io";
            template = "dyndns";
            acl = [ "bernie_acl" ];
          }
          {
            domain = "turingmachine.thalheim.io";
            file = dyndns "turingmachine.thalheim.io";
            template = "dyndns";
            acl = [ "turingmachine_acl" ];
          }
          {
            domain = "blob64.thalheim.io";
            file = dyndns "blob64.thalheim.io";
            template = "dyndns";
            acl = [ "blob64_acl" ];
          }
          {
            domain = "rauter.thalheim.io";
            file = dyndns "rauter.thalheim.io";
            template = "dyndns";
            acl = [ "rauter_acl" ];
          }
        ]
        ++ builtins.map (name: {
          domain = "_acme-challenge.${name}";

          file = "${pkgs.writeText "_acme-challenge.${name}.zone" ''
            @ 3600 IN SOA _acme-challenge.${name}. ns1.thalheim.io. 2021013110 7200 3600 86400 3600

            $TTL 600

            @ IN NS ns1.thalheim.io.
          ''}";

          template = "acme";
        }) ((builtins.attrNames config.security.acme.certs) ++ [ "devkid.net" ]);
    };
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
