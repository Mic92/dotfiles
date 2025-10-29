{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.phpldapadmin;

  phpldapadmin = pkgs.callPackage ./package.nix { };

  # State directory for runtime data
  stateDir = "/var/lib/phpldapadmin";

  # Database configuration
  dbConfig =
    if cfg.database.createLocally then
      {
        DB_CONNECTION = "pgsql";
        DB_HOST = "/run/postgresql";
        DB_PORT = "5432";
        DB_DATABASE = cfg.database.name;
        DB_USERNAME = cfg.database.user;
        DB_PASSWORD = ""; # Unix socket auth
      }
    else
      {
        DB_CONNECTION = cfg.database.type;
        DB_HOST = cfg.database.host;
        DB_PORT = toString cfg.database.port;
        DB_DATABASE = cfg.database.name;
        DB_USERNAME = cfg.database.user;
        DB_PASSWORD = cfg.database.passwordFile;
      };

in
{
  options.services.phpldapadmin = {
    enable = lib.mkEnableOption "phpLDAPadmin, a web-based LDAP administration interface";

    domain = lib.mkOption {
      type = lib.types.str;
      example = "ldap.example.com";
      description = "Domain to serve phpLDAPadmin on";
    };

    url = lib.mkOption {
      type = lib.types.str;
      default = "https://${cfg.domain}";
      defaultText = lib.literalExpression ''"https://''${config.services.phpldapadmin.domain}"'';
      description = "Full URL of the phpLDAPadmin instance";
    };

    appKey = lib.mkOption {
      type = lib.types.str;
      description = ''
        Laravel application key (base64-encoded 32-character string).
        Generate with: `php artisan key:generate --show`
        Store in a secret management system and reference here.
      '';
    };

    poolSettings = lib.mkOption {
      type =
        with lib.types;
        attrsOf (oneOf [
          str
          int
          bool
        ]);
      default = {
        "pm" = "dynamic";
        "pm.max_children" = 32;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 2;
        "pm.max_spare_servers" = 4;
        "pm.max_requests" = 500;
      };
      description = "Options for phpLDAPadmin's PHP-FPM pool.";
    };

    database = {
      createLocally = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Create a local PostgreSQL database automatically";
      };

      type = lib.mkOption {
        type = lib.types.enum [
          "pgsql"
          "mysql"
          "sqlite"
        ];
        default = "pgsql";
        description = "Database type";
      };

      name = lib.mkOption {
        type = lib.types.str;
        default = "phpldapadmin";
        description = "Database name";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "phpldapadmin";
        description = "Database user";
      };

      host = lib.mkOption {
        type = lib.types.str;
        default = "localhost";
        description = "Database host (for external database)";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 5432;
        description = "Database port (for external database)";
      };

      passwordFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to file containing database password (for external database)";
      };
    };

    extraEnvVars = lib.mkOption {
      type = lib.types.lines;
      default = "";
      example = ''
        MAIL_MAILER=smtp
        MAIL_HOST=smtp.example.com
      '';
      description = "Extra environment variables to add to .env file";
    };

    nginx = {
      enable = lib.mkEnableOption "nginx virtual host for phpLDAPadmin" // {
        default = true;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Create local PostgreSQL database if requested
    services.postgresql = lib.mkIf cfg.database.createLocally {
      enable = true;
      ensureDatabases = [ cfg.database.name ];
      ensureUsers = [
        {
          name = cfg.database.user;
          ensureDBOwnership = true;
        }
      ];
    };

    services.phpfpm.pools.phpldapadmin = {
      user = "phpldapadmin";
      group = "phpldapadmin";

      phpPackage = phpldapadmin.php;

      settings = {
        "listen.owner" = config.services.nginx.user;
        "listen.group" = config.services.nginx.group;
      }
      // cfg.poolSettings;
    };

    services.nginx = lib.mkIf cfg.nginx.enable {
      enable = true;
      virtualHosts.${cfg.domain} = {
        root = "${stateDir}/app/public";

        locations = {
          "/" = {
            index = "index.php";
            tryFiles = "$uri $uri/ /index.php?$query_string";
          };

          "~ \\.php$" = {
            extraConfig = ''
              fastcgi_pass unix:${config.services.phpfpm.pools.phpldapadmin.socket};
              fastcgi_index index.php;
              fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
              include ${pkgs.nginx}/conf/fastcgi_params;
              include ${pkgs.nginx}/conf/fastcgi.conf;
            '';
          };

          "~ /\\.(?!well-known)" = {
            extraConfig = "deny all;";
          };
        };

        extraConfig = ''
          add_header X-Frame-Options "SAMEORIGIN";
          add_header X-Content-Type-Options "nosniff";

          client_max_body_size 100M;
        '';
      };
    };

    systemd.services.phpldapadmin-setup = {
      description = "phpLDAPadmin setup and maintenance";
      requiredBy = [ "phpfpm-phpldapadmin.service" ];
      before = [ "phpfpm-phpldapadmin.service" ];
      after = lib.optional cfg.database.createLocally "postgresql.service";
      wants = lib.optional cfg.database.createLocally "postgresql.service";
      partOf = [ "phpfpm-phpldapadmin.service" ];
      restartTriggers = [ phpldapadmin ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "phpldapadmin";
        Group = "phpldapadmin";
        StateDirectory = "phpldapadmin";
        WorkingDirectory = stateDir;
        ReadWritePaths = [ stateDir ];
      };

      path = [ pkgs.rsync ] ++ lib.optional cfg.database.createLocally config.services.postgresql.package;

      script = ''
        set -e

        # Create storage directories if they don't exist
        mkdir -p ${stateDir}/storage/{app,framework/{cache,sessions,views},logs}
        mkdir -p ${stateDir}/bootstrap/cache

        # Build .env file in state directory
        cat > ${stateDir}/.env <<EOF
        APP_NAME="phpLDAPadmin"
        APP_ENV=production
        APP_KEY=$(< ${cfg.appKey})
        APP_DEBUG=false
        APP_URL=${cfg.url}

        LOG_CHANNEL=syslog
        LOG_LEVEL=info

        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n}=${v}") dbConfig)}

        ${cfg.extraEnvVars}
        EOF

        # Create .env symlink in app directory by temporarily making it writable
        # Laravel expects .env in the application root
        if [ ! -e ${stateDir}/app ]; then
          mkdir -p ${stateDir}/app
        fi
        ln -sfn ${stateDir}/.env ${stateDir}/app/.env

        # Symlink storage and bootstrap directories
        ln -sfn ${stateDir}/storage ${stateDir}/app/storage
        ln -sfn ${stateDir}/bootstrap ${stateDir}/app/bootstrap

        # Copy application files to writable location, preserving structure
        # Use rsync to efficiently sync only changed files
        ${pkgs.rsync}/bin/rsync -a --delete \
          --exclude=storage \
          --exclude=bootstrap/cache \
          --exclude=.env \
          ${phpldapadmin}/share/php/phpldapadmin/ ${stateDir}/app/

        # Run artisan commands from the writable app directory
        cd ${stateDir}/app

        # Run database migrations
        ${phpldapadmin.php}/bin/php artisan migrate --force

        # Clear and cache config
        ${phpldapadmin.php}/bin/php artisan config:clear
        ${phpldapadmin.php}/bin/php artisan route:clear
        ${phpldapadmin.php}/bin/php artisan view:clear
        ${phpldapadmin.php}/bin/php artisan config:cache
        ${phpldapadmin.php}/bin/php artisan route:cache
        ${phpldapadmin.php}/bin/php artisan view:cache
      '';
    };

    users.users.phpldapadmin = {
      group = "phpldapadmin";
      isSystemUser = true;
    };

    users.groups.phpldapadmin = { };
  };
}
