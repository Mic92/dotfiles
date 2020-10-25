{ config, lib, pkgs, ... }:

{
  services.home-assistant.config.mqtt = {
    username = "!secret mqtt_username";
    password = "!secret mqtt_password";
  };
  networking.firewall.allowedTCPPorts = [ 8883 ];

  services.nginx = {
    appendConfig = ''
      stream {
        server {
          listen 8883 ssl;
          ssl_certificate /var/lib/acme/thalheim.io/fullchain.pem;
          ssl_certificate_key /var/lib/acme/thalheim.io/key.pem;
          ssl_trusted_certificate /var/lib/acme/thalheim.io/fullchain.pem;
          proxy_pass [::1]:1883;
        }
      }
    '';
    virtualHosts."mqtt.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
    };
  };
}
