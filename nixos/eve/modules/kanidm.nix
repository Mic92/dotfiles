{ config, ... }: {
  services.kanidm.enableServer = true;
  services.kanidm.serverSettings = {
    tls_chain = "/var/lib/acme/kanidm.thalheim.io/fullchain.pem";
    tls_key = "/var/lib/acme/kanidm.thalheim.io/privkey.pem";
    bindaddress = "[::1]:3013";
    origin = "https://kanidm.thalheim.io";
  };

  security.acme.certs."kanidm.thalheim.io" = {
    postRun = "systemctl restart kanidm.service";
    group = "kanidm";
    dnsProvider = "rfc2136";
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
  };

  services.nginx = {
    virtualHosts."kanidm.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://[::1]:3013";
      };
    };
  };
}
