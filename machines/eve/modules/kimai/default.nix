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

  # Kimai's LDAP support requires laminas/laminas-ldap which isn't in
  # the default composer.json.  Build it as a separate composer package
  # and merge its vendor directory into the kimai derivation.
  ldapDeps = pkgs.callPackage ./ldap-deps { inherit (pkgs) php; };
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

  # Render the LDAP bind password into an env file that both the init
  # service (via systemd EnvironmentFile) and PHP-FPM workers (via
  # Symfony's DotEnv) can read.
  sops.templates."kimai-ldap.env" = {
    owner = user;
    content = ''
      KIMAI_LDAP_PASSWORD=${config.sops.placeholder.kimai-ldap-password}
    '';
  };

  # After the upstream init service writes .env, append the LDAP
  # password so PHP-FPM workers (which read .env via Symfony's DotEnv
  # rather than inheriting systemd env vars) can resolve it.
  systemd.services."kimai-init-${hostName}".serviceConfig.ExecStartPost = [
    (pkgs.writeShellScript "kimai-append-ldap-env" ''
      cat ${config.sops.templates."kimai-ldap.env".path} >> ${stateDir}/.env
    '')
  ];

  # Create the admin user on first boot.
  systemd.services."kimai-admin-${hostName}" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "kimai-init-${hostName}.service" ];
    requires = [ "kimai-init-${hostName}.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = config.services.nginx.group;
      EnvironmentFile = [ config.sops.templates."kimai-ldap.env".path ];
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
      package = pkgs.kimai.overrideAttrs (old: {
        postInstall = old.postInstall + ''
          # Merge laminas-ldap vendor files for LDAP authentication support.
          cp -r ${ldapDeps}/share/php/kimai-ldap-deps/vendor/laminas \
            "$out"/share/php/kimai/vendor/
          # Rebuild autoloader to pick up laminas classes.
          (cd "$out"/share/php/kimai && php -d memory_limit=384M \
            ${pkgs.phpPackages.composer}/bin/composer dump-autoload --no-dev --classmap-authoritative)
        '';
      });
      environmentFile = config.sops.templates."kimai-ldap.env".path;
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
            usernameAttribute = "mail";
            filter = "(&(objectClass=inetOrgPerson)(memberOf=cn=kimai,ou=groups,dc=eve))";
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
