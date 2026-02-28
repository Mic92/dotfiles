# Kimai time tracking — using PostgreSQL instead of the nixpkgs module's
# MySQL-only approach, with LDAP authentication against Eve's OpenLDAP.
#
# We re-use the upstream kimai package but override its PHP to include
# pdo_pgsql and ldap, then wire up the init service, PHP-FPM pool, nginx
# vhost and PostgreSQL database ourselves.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  hostName = "kimai.thalheim.io";
  stateDir = "/var/lib/kimai/${hostName}";
  user = "kimai";

  # The upstream package only ships pdo_mysql; we need pdo_pgsql + ldap.
  kimaiPkg = pkgs.kimai.override {
    php = pkgs.kimai.php.buildEnv {
      extensions =
        { enabled, all }:
        enabled
        ++ (with all; [
          gd
          intl
          ldap
          mbstring
          pdo
          pdo_pgsql
          tokenizer
          xsl
          zip
        ]);
      extraConfig = ''
        memory_limit=384M
      '';
    };
  };

  kimaiConfig = pkgs.writeTextFile {
    name = "kimai-local.yaml";
    text = lib.generators.toYAML { } {
      kimai = {
        ldap = {
          activate = true;
          connection = {
            host = "127.0.0.1";
            port = 389;
            # Symfony resolves %env()% from the process environment.
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

  # Wrap the package so .env and var/ point into the state directory.
  wrappedKimai = pkgs.stdenv.mkDerivation {
    pname = "kimai-${hostName}";
    inherit (kimaiPkg) version;
    src = kimaiPkg;
    installPhase = ''
      mkdir -p $out
      cp -r * $out/

      ln -sf ${stateDir}/.env $out/share/php/kimai/.env

      rm -rf $out/share/php/kimai/var
      ln -s ${stateDir} $out/share/php/kimai/var

      ln -s ${kimaiConfig} $out/share/php/kimai/config/packages/local.yaml
    '';
  };

  dbName = "kimai";
  dbUser = user;

  vars = config.clan.core.vars.generators.kimai.files;

  # Doctrine-style DSN for PostgreSQL over unix socket.
  dbUri = "pgsql://${dbUser}@localhost:5432/${dbName}?charset=utf8&serverVersion=${config.services.postgresql.package.version}";
in
{
  clan.core.vars.generators.kimai = {
    files.admin-password = {
      owner = user;
    };
    runtimeInputs = [ pkgs.pwgen ];
    script = ''
      pwgen -s 20 1 > "$out"/admin-password
    '';
  };

  sops.secrets.kimai-ldap-password.owner = user;

  services.postgresql.ensureDatabases = [ dbName ];
  services.postgresql.ensureUsers = [
    {
      name = dbUser;
      ensureDBOwnership = true;
    }
  ];

  users.users.${user} = {
    group = config.services.nginx.group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d '${stateDir}' 0770 ${user} ${config.services.nginx.group} - -"
  ];

  systemd.services."kimai-init-${hostName}" = {
    wantedBy = [ "multi-user.target" ];
    before = [ "phpfpm-kimai-${hostName}.service" ];
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
    script = ''
      set -eu

      envFile="${stateDir}/.env"
      appSecretFile="${stateDir}/.app_secret"

      oldUmask=$(umask)
      umask 177

      if ! [ -e "$appSecretFile" ]; then
        tr -dc A-Za-z0-9 </dev/urandom | head -c 20 >"$appSecretFile"
      fi

      cat >"$envFile" <<EOF
      DATABASE_URL=${dbUri}
      MAILER_FROM=kimai@thalheim.io
      MAILER_URL=null://null
      APP_ENV=prod
      APP_SECRET=$(cat "$appSecretFile")
      CORS_ALLOW_ORIGIN=^https?://localhost(:[0-9]+)?\$
      KIMAI_LDAP_PASSWORD=$(cat ${config.sops.secrets.kimai-ldap-password.path})
      EOF

      umask $oldUmask

      ${wrappedKimai}/bin/console lint:yaml --parse-tags \
        ${wrappedKimai}/share/php/kimai/config

      ${wrappedKimai}/bin/console cache:clear --env=prod
      ${wrappedKimai}/bin/console kimai:install --no-cache
      ${wrappedKimai}/bin/console cache:warmup --env=prod

      # Create admin user on first run. The command fails if the user already
      # exists, so we guard with a marker file.
      if ! [ -e "${stateDir}/.admin-created" ]; then
        adminPassword=$(cat ${vars.admin-password.path})
        ${wrappedKimai}/bin/console kimai:user:create \
          admin admin@thalheim.io ROLE_SUPER_ADMIN "$adminPassword"
        touch "${stateDir}/.admin-created"
      fi
    '';
    serviceConfig = {
      Type = "oneshot";
      User = user;
      Group = config.services.nginx.group;
    };
  };

  services.phpfpm.pools."kimai-${hostName}" = {
    phpPackage = kimaiPkg.php;
    user = user;
    group = config.services.nginx.group;
    settings = {
      "listen.owner" = config.services.nginx.user;
      "listen.group" = config.services.nginx.group;
      "pm" = "dynamic";
      "pm.max_children" = 32;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 2;
      "pm.max_spare_servers" = 4;
      "pm.max_requests" = 500;
    };
  };

  services.nginx.virtualHosts.${hostName} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    serverName = hostName;
    root = "${wrappedKimai}/share/php/kimai/public";
    extraConfig = ''
      index index.php;
    '';
    locations = {
      "/" = {
        priority = 200;
        extraConfig = ''
          try_files $uri /index.php$is_args$args;
        '';
      };
      "~ ^/index\\.php(/|$)" = {
        priority = 500;
        extraConfig = ''
          fastcgi_split_path_info ^(.+\.php)(/.+)$;
          fastcgi_pass unix:${config.services.phpfpm.pools."kimai-${hostName}".socket};
          fastcgi_index index.php;
          include "${config.services.nginx.package}/conf/fastcgi.conf";
          fastcgi_param PATH_INFO $fastcgi_path_info;
          fastcgi_param PATH_TRANSLATED $document_root$fastcgi_path_info;
          fastcgi_param HTTP_PROXY "";
          fastcgi_intercept_errors off;
          fastcgi_buffer_size 16k;
          fastcgi_buffers 4 16k;
          fastcgi_connect_timeout 300;
          fastcgi_send_timeout 300;
          fastcgi_read_timeout 300;
        '';
      };
      "~ \\.php$" = {
        priority = 800;
        extraConfig = ''
          return 404;
        '';
      };
    };
  };
}
