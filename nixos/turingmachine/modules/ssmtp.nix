{ config, ... }: {
  services.ssmtp = {
    enable = true;
    authPassFile = config.sops.secrets.smtp-authpass.path;
    authUser = "joerg@higgsboson.tk";
    hostName = "mail.thalheim.io:587";
    domain = "thalheim.io";
    root = "joerg@thalheim.io";
    useSTARTTLS = true;
  };
  sops.secrets.smtp-authpass = {};
}
