{ pkgs, ... }:
let
  conf = pkgs.writeText "Caddyfile" ''
    http://0.0.0.0:8080 {
      file_server /* browse {
        root /home/joerg/web
      }
    }
  '';
in
{
  systemd.services.caddy = {
    description = "Caddy web server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.caddy}/bin/caddy run --config=${conf} --adapter caddyfile";
      User = "joerg";
      AmbientCapabilities = "cap_net_bind_service";
    };
  };
}
