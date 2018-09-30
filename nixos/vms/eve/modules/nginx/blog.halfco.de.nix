{
  services.nginx = {
    virtualHosts."blog.halfco.de" = {
      useACMEHost = "halfco.de";
      forceSSL = true;
      root = "/srv/http/blog.halfco.de";
      locations."/".extraConfig = ''
        proxy_set_header   X-Real-IP $remote_addr;
        proxy_set_header   Host      $http_host;
        # FIXME
        proxy_pass         http://localhost:9000;
      '';
    };
    virtualHosts."www.blog.halfco.de" = {
      globalRedirect = "blog.halfco.de";
    };
  };
}
