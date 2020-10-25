{ config, lib, pkgs, ... }:

{
  services.home-assistant.config.mqtt = {
    username = "!secret mqtt_username";
    password = "!secret mqtt_password";
  };
  networking.firewall.allowedTCPPorts = [ 8883 ];

  #services.mosquitto = {
  #  enable = true;
  #  users = {
  #    rock = {
  #      acl = [ "topic readwrite homeassistant/#" ];
  #      hashedPassword = "$6$KFZx41e5P6TAGPP5$Qn0UXfC2jTQAewurEMBu8IJVHM+SK5aFIe/fZoFvJBtIgh0ISg/iAncoIuOrB4QeI3XFat0Ty8538YjvKnNx0A==";
  #    };
  #    homeassistant = {
  #      acl = [ "pattern readwrite homeassistant/#" ];
  #      hashedPassword = "$6$9vuXRsVInILgxSWy$2NlAmS6rVwZFc+O/0WyaK6Kzf8UIxQtoaemn8Yz5OWOKm45P3fbAyF1fQcBVF5jCvjXVqZ62Vixm80ECUwUjtw==";
  #    };
  #  };
  #};

  services.nginx = {
    streamConfig = ''
      server {
        listen 8883 ssl;
        ssl_certificate /var/lib/acme/thalheim.io/fullchain.pem;
        ssl_certificate_key /var/lib/acme/thalheim.io/key.pem;
        ssl_trusted_certificate /var/lib/acme/thalheim.io/fullchain.pem;
        proxy_pass [::1]:1883;
      }
    '';
    virtualHosts."mqtt.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
    };
  };
}
