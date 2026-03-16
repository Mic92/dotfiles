# Nostr blob storage server (Blossom / NIP-96 protocols)
#
#   https://nostr-files.thalheim.io
#
# Test connectivity:
#   curl https://nostr-files.thalheim.io/
#   curl -H "Accept: application/nostr+json" https://nostr-files.thalheim.io/.well-known/nostr/nip96.json
{
  pkgs,
  ...
}:
let
  route96 = pkgs.callPackage ../../../pkgs/route96 { };
  domain = "nostr-files.thalheim.io";
  port = 8396;
  dataDir = "/var/lib/route96";

  configFile = pkgs.writeText "route96-config.yaml" ''
    listen: "127.0.0.1:${toString port}"
    database: "mysql://route96@localhost/route96?socket=/run/mysqld/mysqld.sock"
    storage_dir: "${dataDir}/files"
    max_upload_bytes: 104857600
    public_url: "https://${domain}"
    delete_unaccessed_days: 90
  '';
in
{
  # ── MySQL database ──────────────────────────────────────────────────────

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = [ "route96" ];
    ensureUsers = [
      {
        name = "route96";
        ensurePermissions = {
          "route96.*" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  # ── route96 service ─────────────────────────────────────────────────────

  systemd.services.route96 = {
    description = "Route96 Nostr blob storage";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network.target"
      "mysql.service"
    ];
    requires = [ "mysql.service" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${route96}/bin/route96 --config ${configFile}";
      Restart = "on-failure";
      RestartSec = 5;
      # Working directory determines where index.html is loaded from
      WorkingDirectory = "${route96}/share/route96";

      StateDirectory = "route96";
      User = "route96";
      Group = "route96";

      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      ReadWritePaths = [ dataDir ];
    };

    preStart = ''
      mkdir -p ${dataDir}/files
    '';
  };

  users.users.route96 = {
    isSystemUser = true;
    group = "route96";
  };
  users.groups.route96 = { };

  # ── nginx reverse proxy ─────────────────────────────────────────────────

  services.nginx.virtualHosts.${domain} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
        client_max_body_size 100m;
      '';
    };
  };
}
