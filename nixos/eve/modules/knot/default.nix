{ pkgs
, config
, inputs
, ...
}:
let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = config.networking.eve.ipv6.address;
  acmeChallenge = domain:
    pkgs.writeText "_acme-challenge.${domain}.zone" ''
      @ 3600 IN SOA _acme-challenge.${domain}. ns1.thalheim.io. 2021013110 7200 3600 86400 3600

      $TTL 600

      @ IN NS ns1.thalheim.io.
    '';
  dyndns = domain:
    pkgs.writeText "${domain}.zone" ''
      @ 3600 IN SOA ${domain}. ns1.thalheim.io. 2021013112 7200 3600 86400 3600

      $TTL 300

      @ IN NS ns1.thalheim.io.
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
    keyFiles = [
      config.sops.secrets."knot-keys.conf".path
    ];
    settings = {
      server = {
        listen = [ "${ip4}@53" "${ip6}@53" ];
      };

      remote = [{
        id = "he_ip4";
        address = "216.218.130.2";
        # does not accept NOTIFY yet
        #- id: he_ip6
        #  address: 2001:470:100::2
      }];

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
        {
          id = "bbc_acl";
          key = "bbc";
          action = "update";
        }
      ];

      mod-rrl = [{
        id = "default";
        rate-limit = 200;
        slip = 2;
      }];

      policy = [{
        id = "default";
        algorithm = "RSASHA256";
        ksk-size = 4096;
        zsk-size = 2048;
      }];


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
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "master";
          semantic-checks = "on";
          notify = [ "he_ip4" ];
          acl = [ "he_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "acme";
          semantic-checks = "on";
          acl = [ "acme_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "dyndns";
          semantic-checks = "on";
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "bernie";
          semantic-checks = "on";
          acl = [ "bernie_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "matchbox";
          semantic-checks = "on";
          acl = [ "matchbox_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
        {
          id = "bbc";
          semantic-checks = "on";
          acl = [ "bbc_acl" ];
          zonefile-sync = "-1";
          zonefile-load = "difference";
          journal-content = "changes";
        }
      ];
      zone = [
        {
          domain = "thalheim.io";
          file = "${./thalheim.io.zone}";
          template = "master";
        }
        {
          domain = "tierheilpraxis-jessican.de";
          file = "${./tierheilpraxis-jessican.de.zone}";
          template = "master";
        }
        {
          domain = "lekwati.com";
          file = "${./lekwati.com.zone}";
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
          domain = "bbc.lekwati.com";
          file = "${dyndns "bbc.lekwati.com"}";
          template = "dyndns";
          acl = [ "bbc_acl" ];
        }
        {
          domain = "matchbox.thalheim.io";
          file = "${dyndns "matchbox.thalheim.io"}";
          template = "dyndns";
          acl = [ "matchbox_acl" ];
        }
        {
          domain = "bernie.thalheim.io";
          file = "${dyndns "bernie.thalheim.io"}";
          template = "dyndns";
          acl = [ "bernie_acl" ];
        }
        {
          domain = "turingmachine.thalheim.io";
          file = "${dyndns "turingmachine.thalheim.io"}";
          template = "dyndns";
          acl = [ "turingmachine_acl" ];
        }
        {
          domain = "blob64.thalheim.io";
          file = "${dyndns "blob64.thalheim.io"}";
          template = "dyndns";
          acl = [ "blob64_acl" ];
        }
        {
          domain = "rauter.thalheim.io";
          file = "${dyndns "rauter.thalheim.io"}";
          template = "dyndns";
          acl = [ "rauter_acl" ];
        }
        {
          domain = "_acme-challenge.thalheim.io";
          file = "${acmeChallenge "thalheim.io"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.anon.thalheim.io";
          file = "${acmeChallenge "anon.thalheim.io"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.imap.thalheim.io";
          file = "${acmeChallenge "imap.thalheim.io"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.mail.thalheim.io";
          file = "${acmeChallenge "mail.thalheim.io"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.lekwati.com";
          file = "${acmeChallenge "lekwati.com"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.devkid.net";
          file = "${acmeChallenge "devkid.net"}";
          template = "acme";
        }
        {
          domain = "_acme-challenge.imap.devkid.net";
          file = "${acmeChallenge "imap.devkid.net"}";
          template = "acme";
        }
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
