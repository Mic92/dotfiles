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

  services.nginx.virtualHosts."influxdb.thalheim.io".useACMEHost = "influxdb.thalheim.io";
}
