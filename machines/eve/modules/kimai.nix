# Kimai time tracking — thin wrapper around the nixpkgs module, adding
# ACME TLS, LDAP authentication against Eve's OpenLDAP, and an admin
# user that is created on first boot.
{
  config,
  pkgs,
  ...
}:
let
  hostName = "kimai.thalheim.io";
  stateDir = "/var/lib/kimai/${hostName}";
  user = "kimai";
  vars = config.clan.core.vars.generators.kimai.files;
in
{
  clan.core.vars.generators.kimai = {
    files.admin-password.owner = user;
    runtimeInputs = [ pkgs.pwgen ];
    script = ''
      pwgen -s 20 1 > "$out"/admin-password
    '';
  };

  sops.secrets.kimai-ldap-password.owner = user;

  # Write an environment file that exposes the LDAP bind password so
  # Symfony's %env(KIMAI_LDAP_PASSWORD)% can resolve it at runtime.
  systemd.services."kimai-env-${hostName}" = {
    wantedBy = [ "multi-user.target" ];
    before = [ "kimai-init-${hostName}.service" ];
    after = [ "sops-install-secrets.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = config.services.nginx.group;
      UMask = "0177";
    };
    script = ''
      cat > ${stateDir}/kimai.env <<EOF
      KIMAI_LDAP_PASSWORD=$(cat ${config.sops.secrets.kimai-ldap-password.path})
      EOF
    '';
  };

  # Create the admin user on first boot.
  systemd.services."kimai-admin-${hostName}" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "kimai-init-${hostName}.service" ];
    requires = [ "kimai-init-${hostName}.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = config.services.nginx.group;
    };
    script =
      let
        kimai = config.services.kimai.sites.${hostName}.package;
        wrappedKimai = pkgs.stdenv.mkDerivation {
          pname = "kimai-${hostName}";
          inherit (kimai) version;
          src = kimai;
          installPhase = ''
            mkdir -p $out
            cp -r * $out/
            ln -sf ${stateDir}/.env $out/share/php/kimai/.env
            rm -rf $out/share/php/kimai/var
            ln -s ${stateDir} $out/share/php/kimai/var
          '';
        };
      in
      ''
        set -eu
        if ! [ -e "${stateDir}/.admin-created" ]; then
          adminPassword=$(cat ${vars.admin-password.path})
          ${wrappedKimai}/bin/console kimai:user:create \
            admin admin@thalheim.io ROLE_SUPER_ADMIN "$adminPassword"
          touch "${stateDir}/.admin-created"
        fi
      '';
  };

  services.kimai = {
    sites.${hostName} = {
      environmentFile = "${stateDir}/kimai.env";
      settings = {
        kimai.ldap = {
          activate = true;
          connection = {
            host = "127.0.0.1";
            port = 389;
            username = "cn=kimai,ou=system,ou=users,dc=eve";
            password = "%env(KIMAI_LDAP_PASSWORD)%";
            bindRequiresDn = true;
          };
          user = {
            baseDn = "ou=users,dc=eve";
            usernameAttribute = "uid";
            filter = "(&(objectClass=inetOrgPerson))";
            attributes = [
              {
                ldap_attr = "uid";
                user_method = "setUserIdentifier";
              }
              {
                ldap_attr = "mail";
                user_method = "setEmail";
              }
              {
                ldap_attr = "cn";
                user_method = "setAlias";
              }
            ];
          };
          role = {
            baseDn = "ou=groups,dc=eve";
            filter = "(&(objectClass=groupOfNames))";
            userDnAttribute = "member";
            nameAttribute = "cn";
            groups = [
              {
                ldap_value = "admins";
                role = "ROLE_SUPER_ADMIN";
              }
            ];
          };
        };
      };
    };
  };

  services.nginx.virtualHosts.${hostName} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
  };
}
