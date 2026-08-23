# lldap with a patch to accept password hashes imported from OpenLDAP,
# upgrading them to OPAQUE records on first bind (see patches/).
#
# The key_seed is shared between eve and eva: the replica receives a copy
# of the primary's database (see sync-primary.nix/sync-replica.nix) and
# the password records are only usable with the same key.
{ pkgs, config, ... }:
let
  lldap = pkgs.lldap.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./patches/0001-Accept-imported-legacy-password-hashes-upgrade-to-OP.patch
    ];
    cargoHash = "sha256-Ik8IZnADCcpn0WBfptHi7sf0uREJjOE2jCV8LP/EyBE=";
  });
  vars = config.clan.core.vars.generators.lldap;
  credentialsDir = "/run/credentials/lldap.service";
in
{
  clan.core.vars.generators.lldap = {
    share = true;
    files.key-seed = { };
    files.jwt-secret = { };
    files.admin-password = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.xkcdpass
      pkgs.openssl
    ];
    script = ''
      xkcdpass -d - -n 4 | tr -d '\n' > "$out/admin-password"
      openssl rand -hex 32 | tr -d '\n' > "$out/key-seed"
      openssl rand -hex 32 | tr -d '\n' > "$out/jwt-secret"
    '';
  };

  services.lldap = {
    enable = true;
    package = lldap;
    database.type = "postgresql";
    environment = {
      LLDAP_KEY_SEED_FILE = "${credentialsDir}/key-seed";
      LLDAP_JWT_SECRET_FILE = "${credentialsDir}/jwt-secret";
      LLDAP_LDAP_USER_PASS_FILE = "${credentialsDir}/admin-password";
    };
    settings = {
      ldap_base_dn = "dc=eve";
      force_ldap_user_pass_reset = "always";
    };
  };

  systemd.services.lldap.serviceConfig.LoadCredential = [
    "key-seed:${vars.files.key-seed.path}"
    "jwt-secret:${vars.files.jwt-secret.path}"
    "admin-password:${vars.files.admin-password.path}"
  ];

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
    3890 # ldap
    17170 # web ui
  ];
}
