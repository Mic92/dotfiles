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

  # PHP auto-prepend file that:
  #  1. Registers the laminas-ldap PSR-4 namespace (not in composer's
  #     autoloader since we side-loaded it)
  #  2. Injects KIMAI_LDAP_PASSWORD from the sops-rendered file into
  #     the process environment so Symfony's %env()% resolves it
  kimaiPrepend = pkgs.writeText "kimai-prepend.php" ''
    <?php
    // Register laminas-ldap autoloader
    spl_autoload_register(function ($class) {
        $prefix = 'Laminas\\Ldap\\';
        if (strncmp($prefix, $class, strlen($prefix)) !== 0) return;
        $file = __DIR__ . '/vendor/laminas/laminas-ldap/src/'
              . str_replace('\\', '/', substr($class, strlen($prefix))) . '.php';
        if (file_exists($file)) require $file;
    });

    // Load LDAP password from sops-rendered env file
    $envFile = '${config.sops.templates."kimai-ldap.env".path}';
    if (file_exists($envFile) && !getenv('KIMAI_LDAP_PASSWORD')) {
        foreach (file($envFile, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
            if (strpos($line, '=') !== false) {
                putenv($line);
                [$k, $v] = explode('=', $line, 2);
                $_ENV[$k] = $v;
                $_SERVER[$k] = $v;
            }
        }
    }
  '';
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

  # Render the LDAP bind password into an env file.
  sops.templates."kimai-ldap.env" = {
    owner = user;
    content = ''
      KIMAI_LDAP_PASSWORD=${config.sops.placeholder.kimai-ldap-password}
    '';
  };

  # The upstream init service needs the env var for cache:warmup.
  systemd.services."kimai-init-${hostName}".serviceConfig.EnvironmentFile =
    config.sops.templates."kimai-ldap.env".path;

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
          # Install the auto-prepend file next to the kimai root so the
          # relative vendor/ path resolves correctly.
          cp ${kimaiPrepend} "$out"/share/php/kimai/kimai-prepend.php
        '';
      });
      environmentFile = config.sops.templates."kimai-ldap.env".path;
      poolConfig = {
        "pm" = "dynamic";
        "pm.max_children" = 32;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 2;
        "pm.max_spare_servers" = 4;
        "pm.max_requests" = 500;
        "php_admin_value[auto_prepend_file]" = "${
          config.services.kimai.sites.${hostName}.package
        }/share/php/kimai/kimai-prepend.php";
      };
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
