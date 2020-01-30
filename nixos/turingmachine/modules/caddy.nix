{ pkgs, ... }: let
  conf = pkgs.writeText "Caddyfile" ''
    0.0.0.0 {
      timeouts 0
      tls off
      #markdown
      browse
      root /home/joerg/web

      basicauth /privat root cakeistasty
      basicauth /private root kuchenistlecker
    }
  '';
in {
  systemd.services.caddy = {
    description = "Caddy web server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''${pkgs.caddy}/bin/caddy -conf=${conf} -agree'';
      User = "joerg";
      AmbientCapabilities = "cap_net_bind_service";
    };
  };
}
