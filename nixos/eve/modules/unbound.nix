{ pkgs, lib, config, ... }: {
  services.unbound = {
    enable = true;
    settings = {
      server = {
        access-control = [
          "::/0 allow"
        ];
        prefetch = "yes";
        tls-service-key = "/var/lib/acme/dns.thalheim.io/key.pem";
        tls-service-pem = "/var/lib/acme/dns.thalheim.io/fullchain.pem";
        tls-port = 853;
        interface = [
          "127.0.0.1@53"
          "::@853"
          "0.0.0.0@853"
        ];
      };
    };
  };

  # dns.thalheim.io
  networking.firewall.allowedTCPPorts = [ 853 ];

  security.acme.certs."dns.thalheim.io" = {
    postRun = "systemctl restart unbound.service";
    group = "unbound";
    dnsProvider = "rfc2136";
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
  };
}
