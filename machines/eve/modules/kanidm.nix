{ config, ... }:
{
  services.kanidm.enableServer = true;
  services.kanidm.serverSettings = {
    tls_chain = "/var/lib/acme/kanidm.thalheim.io/fullchain.pem";
    tls_key = "/var/lib/acme/kanidm.thalheim.io/key.pem";
    bindaddress = "[::1]:3013";
    ldapbindaddress = "[::1]:3636";
    domain = "kanidm.thalheim.io";
    origin = "https://kanidm.thalheim.io";
    trust_x_forward_for = true;
    db_fs_type = "zfs";

    online_backup = {
      path = "/var/lib/kanidm/backup";
      schedule = "0 0 * * *";
    };
  };

  security.acme.certs."kanidm.thalheim.io" = {
    postRun = "systemctl restart kanidm.service";
    group = "kanidm";
    dnsProvider = "rfc2136";
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
  };

  services.nginx.virtualHosts."kanidm.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "https://[::1]:3013";
    };
  };
}
