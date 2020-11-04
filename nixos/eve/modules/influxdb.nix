{ ... }: {
  services.influxdb = {
    enable = true;
    extraConfig = {
      http = {
        auth-enabled = true;
        https-enabled = true;
        https-certificate = "/var/lib/acme/influxdb.thalheim.io/fullchain.pem";
        https-private-key = "/var/lib/acme/influxdb.thalheim.io/key.pem";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 8086 ];

  security.acme.certs."influxdb.thalheim.io" = {
    postRun = "systemctl restart influxdb.service";
    webroot = "/var/lib/acme/acme-challenge";
    group = "influxdb";
  };
  systemd.services.nginx.serviceConfig.SupplementaryGroups = [ "influxdb" ];

  services.nginx.virtualHosts."influxdb.thalheim.io".useACMEHost = "influxdb.thalheim.io";
}
