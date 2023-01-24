{ config, ... }: {
  services.onlyoffice = {
    enable = true;
    port = 8111;
    hostname = "onlyoffice.thalheim.io";
    jwtSecretFile = config.sops.secrets.onlyoffice-jwd-secret.path;
  };

  sops.secrets.onlyoffice-jwd-secret.owner = "onlyoffice";
}
