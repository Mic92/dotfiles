{ config, ... }:
{
  services.victorialogs = {
    enable = true;
    listenAddress = "127.0.0.1:9428";
    extraOptions = [
      "-retentionPeriod=5d"
      "-retention.maxDiskSpaceUsageBytes=20GiB"
      "-loggerLevel=WARN"
    ];
  };

  # htpasswd with one entry per shipper (own hosts, doctor cluster, makefu's
  # hosts via retiolum). Plaintext for external parties lives in
  # promtail-<name>-password so it can be handed over / rotated separately.
  sops.secrets.promtail-nginx-password.owner = "nginx";

  # external shippers (makefu, doctor-cluster) still push to loki.r
  security.acme.certs."loki.r".server = config.retiolum.ca.acmeURL;
  services.nginx = {
    enable = true;
    virtualHosts."loki.r" = {
      serverName = "loki.r";
      enableACME = true;
      addSSL = true;
      basicAuthFile = config.sops.secrets.promtail-nginx-password.path;
      locations."/" = {
        proxyPass = "http://127.0.0.1:9428";
        extraConfig = ''
          proxy_read_timeout 1800s;
          access_log off;
        '';
      };
      locations."/loki/api/" = {
        proxyPass = "http://127.0.0.1:9428/insert/loki/api/";
        extraConfig = "access_log off;";
      };
      # unauthenticated for telegraf's http_response check
      locations."= /health" = {
        proxyPass = "http://127.0.0.1:9428";
        extraConfig = ''
          auth_basic off;
          access_log off;
        '';
      };
    };
  };
}
