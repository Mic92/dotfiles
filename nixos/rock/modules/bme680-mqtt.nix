{ config, ... }: {
  services.bme680-mqtt.enable = true;
  services.bme680-mqtt.mqtt.passwordFile = config.sops.secrets.mqtt-bme680.path;
  services.bme680-mqtt.mqtt.url = "mqtts://bme680@thalheim.io@mqtt.thalheim.io:8883";
  sops.secrets.mqtt-bme680 = { };
}
