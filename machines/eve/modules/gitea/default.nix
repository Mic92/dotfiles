{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
      host = "/run/postgresql";
      port = 5432;
    };
    mailerPasswordFile = config.sops.secrets.gitea-mail.path;
    settings.mailer = {
      ENABLED = true;
      FROM = "gitea@thalheim.io";
      USER = "gitea@thalheim.io";
      HOST = "mail.thalheim.io:587";
      SENDMAIL_PATH = "/run/wrappers/bin/sendmail";
    };
    settings.log.LEVEL = "Error";
    settings.service.DISABLE_REGISTRATION = true;
    settings.metrics.ENABLED = true;
    settings.server = {
      DISABLE_ROUTER_LOG = true;
      ROOT_URL = "https://git.thalheim.io";
      HTTP_PORT = 3002;
      DOMAIN = "thalheim.io";
    };
    settings.security = {
      DISABLE_GIT_HOOKS = false;
    };
  };

  systemd.services.gitea = {
    path = [ pkgs.bash ];
    serviceConfig.LimitNOFILE = 65536;
    # Keep the LDAP auth source in sync with lldap. The bind user doubles as
    # the SMTP account, so the mailer password is reused.
    preStart = lib.mkAfter ''
      gitea() { ${lib.getExe config.services.gitea.package} --config ${config.services.gitea.customDir}/conf/app.ini "$@"; }
      ldap_args=(
        --name ldap
        --host 127.0.0.1 --port 3890 --security-protocol unencrypted
        --bind-dn uid=gitea,ou=people,dc=eve
        --bind-password "$(cat ${config.sops.secrets.gitea-mail.path})"
        --user-search-base ou=people,dc=eve
        --user-filter '(&(memberOf=cn=gitea,ou=groups,dc=eve)(|(uid=%[1]s)(mail=%[1]s)))'
        --username-attribute uid --surname-attribute cn --email-attribute mail
        --synchronize-users
      )
      id=$(gitea admin auth list | ${pkgs.gawk}/bin/awk '$2 == "ldap" { print $1 }')
      if [[ -n "$id" ]]; then
        gitea admin auth update-ldap --id "$id" "''${ldap_args[@]}"
      else
        gitea admin auth add-ldap "''${ldap_args[@]}"
      fi
    '';
  };

  systemd.tmpfiles.rules =
    let
      hooks =
        pkgs.runCommand "hooks"
          {
            buildInputs = [ pkgs.bash ];
            nativeBuildInputs = [
              pkgs.makeWrapper
              pkgs.shellcheck
            ];
          }
          ''
            install -D -m755 ${./homepage-hook.sh} $out/bin/homepage
            wrapProgram $out/bin/homepage \
              --set PATH ${
                lib.makeBinPath (
                  with pkgs;
                  [
                    bash
                    coreutils
                    git
                    nix
                    rsync
                  ]
                )
              }

            for bin in $out/bin/*; do
              patchShebangs $bin
              shellcheck $bin
            done
          '';
    in
    [
      "L+ /var/lib/gitea/repositories/mic92/homepage.git/hooks/post-receive.d/homepage - - - - ${hooks}/bin/homepage"
    ];

  sops.secrets.gitea-mail.owner = config.systemd.services.gitea.serviceConfig.User;

  services.lldap.ensureGroups = [ "gitea" ];
  services.lldap.ensureUsers.gitea = {
    email = "gitea@thalheim.io";
    passwordFile = config.sops.secrets.gitea-mail.path;
    groups = [
      "lldap_strict_readonly"
      "mail"
    ];
  };

  nix.settings.allowed-users = [ "gitea" ];

  services.nginx.virtualHosts."git.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3002;
    '';
    locations."= /robots.txt".alias = ./robots.txt;
  };
}
