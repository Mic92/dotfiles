{
  config,
  pkgs,
  lib,
  self,
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
            install -D -m755 ${./retiolum-hook.sh} $out/bin/retiolum
            install -D -m755 ${./irc-hook.sh} $out/bin/irc-notify
            install -D -m755 ${./homepage-hook.sh} $out/bin/homepage
            wrapProgram $out/bin/retiolum \
              --set PATH ${
                lib.makeBinPath (
                  with pkgs;
                  [
                    bash
                    bzip2
                    coreutils
                    git
                    gnugrep
                    gnutar
                    jq
                    nix
                    openssh
                  ]
                )
              }
            wrapProgram $out/bin/irc-notify \
              --set PATH ${
                lib.makeBinPath (
                  with pkgs;
                  [
                    git
                    openssh
                    coreutils
                    gnugrep
                    gnused
                    self.inputs.nur-packages.packages.${pkgs.stdenv.hostPlatform.system}.ircsink
                    bash
                  ]
                )
              }
            cat > $out/bin/irc-stockholm <<EOF
            #!/usr/bin/env bash
            export GIT_URL=https://git.thalheim.io/Mic92/stockholm
            exec $out/bin/irc-notify --server=irc.r --nick=gitea --target="#xxx"
            EOF
            chmod +x $out/bin/irc-stockholm
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
      "L+ /var/lib/gitea/repositories/mic92/stockholm.git/hooks/post-receive.d/retiolum - - - - ${hooks}/bin/retiolum"
      "L+ /var/lib/gitea/repositories/mic92/stockholm.git/hooks/post-receive.d/irc-stockholm - - - - ${hooks}/bin/irc-stockholm"
      "L+ /var/lib/gitea/repositories/mic92/homepage.git/hooks/post-receive.d/homepage - - - - ${hooks}/bin/homepage"
    ];

  sops.secrets.gitea-mail.owner = config.systemd.services.gitea.serviceConfig.User;

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
