{ pkgs, ... }: {
  systemd.services = {
    caddy =
      let
        cfg = pkgs.writeText "Caddyfile" ''
          backup.thalheim.io {
            proxy / 127.0.0.1:5000
            timeouts 0
            tls off
          }
          matchbox.thalheim.io {
            timeouts 0
            tls off
            browse
            root /mnt/hdd
          }
        '';
      in
      {
        description = "Caddy web server";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = ''${pkgs.caddy.bin}/bin/caddy -conf=${cfg} -agree'';
          User = "caddy";
          AmbientCapabilities = "cap_net_bind_service";
        };
      };

    users.users.caddy.isSystemUser = true;
  };
}
