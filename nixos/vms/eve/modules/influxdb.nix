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

  security.acme.certs."influxdb.thalheim.io" = {
    postRun = "systemctl restart influxdb.service";
    webroot = "/var/lib/acme/acme-challenge";
    allowKeysForGroup = true;
    group = "influxdb";
  };
}
