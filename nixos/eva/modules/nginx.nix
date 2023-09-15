{ pkgs
, config
, lib
, ...
}:
let
  conf = pkgs.writeText "ldap.conf" ''
    base dc=eve
    host localhost:389
    pam_login_attribute mail
    pam_filter objectClass=prometheus
  '';
  proxy = upstream: ''
    auth_pam "Ldap password";
    auth_pam_service_name "prometheus";

    proxy_pass       http://@${upstream}/;
    proxy_set_header Host              $host;
    proxy_set_header X-Real-IP         $remote_addr;
    proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Host   $host:443;
    proxy_set_header X-Forwarded-Server $host;
    proxy_set_header X-Forwarded-Port 443;
    proxy_set_header X-Forwarded-Proto $scheme;
  '';
in
{
  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      config.quic = true;
    });
  };

  config = {
    networking.firewall.allowedUDPPorts = [ 443 ]; # quic

    security.pam.services.prometheus.text = ''
      auth required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
      account required ${pkgs.pam_ldap}/lib/security/pam_ldap.so config=${conf}
    '';

    security.acme.certs."prometheus.r".server = config.retiolum.ca.acmeURL;
    security.acme.certs."alertmanager.r".server = config.retiolum.ca.acmeURL;

    services.nginx = {
      package = pkgs.nginxQuic.override {
        modules = [ pkgs.nginxModules.pam ];
      };

      commonHttpConfig = ''
        add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains; preload' always;
      '';

      upstreams = {
        "@prometheus".extraConfig = "server localhost:9090;";
        "@alertmanager".extraConfig = "server localhost:9093;";
      };
      virtualHosts."prometheus.thalheim.io" = {
        forceSSL = true;
        enableACME = true;
        locations."/".extraConfig = proxy "prometheus";
      };
      virtualHosts."prometheus.r" = {
        enableACME = true;
        addSSL = true;
        locations."/".extraConfig = ''
          proxy_pass       http://@prometheus/;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Host $host:80;
          proxy_set_header X-Forwarded-Server $host;
          proxy_set_header X-Forwarded-Port 80;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
      virtualHosts."alertmanager.thalheim.io" = {
        forceSSL = true;
        enableACME = true;
        locations."/".extraConfig = proxy "alertmanager";
      };
      virtualHosts."alertmanager.r" = {
        enableACME = true;
        addSSL = true;
        locations."/".extraConfig = ''
          proxy_pass       http://@alertmanager/;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Host $host:80;
          proxy_set_header X-Forwarded-Server $host;
          proxy_set_header X-Forwarded-Port 80;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };

      virtualHosts."ip2.thalheim.io" = {
        enableACME = true;
        addSSL = true;
        locations."/".extraConfig = ''
          default_type text/plain;
          return 200 "$remote_addr\n";
        '';
      };
    };
  };
}
