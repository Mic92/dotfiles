{ ... }: {
  services.influxdb = {
    enable = true;
    extraConfig = {
      admin = {
        https-enabled = true;
        https-certificate = "/etc/letsencrypt/live/influxdb.thalheim.io-0001/both.pem";
      };

      http = {
        auth-enabled = true;
        https-enabled = true;
        https-certificate = "/etc/letsencrypt/live/influxdb.thalheim.io-0001/fullchain.pem";
        https-private-key = "/etc/letsencrypt/live/influxdb.thalheim.io-0001/privkey.pem";
      };
    };
  };
}
