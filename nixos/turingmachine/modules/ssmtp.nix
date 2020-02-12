{ ... }: {
  services.ssmtp = {
    enable = true;
    authPassFile = toString <secrets/smtp-authpass>;
    authUser = "joerg@higgsboson.tk";
    hostName = "mail.thalheim.io:587";
    domain = "thalheim.io";
    root = "joerg@thalheim.io";
    useSTARTTLS = true;
  };
}
