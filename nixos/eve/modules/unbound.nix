{ pkgs
, config
, ...
}: {
  imports = [
    ../../modules/unbound.nix
  ];
  services.unbound = {
    package = pkgs.unbound-with-systemd.override { withDoH = true; };
    settings = {
      server = {
        access-control = [
          "::/0 allow"
          "0.0.0.0/0 allow"
        ];
        # Too many broken dnssec setups even at big companies such as amazon.
        # Breaks my email setup. Better rely on tls for security.
        val-permissive-mode = "yes";

        tls-service-key = "/var/lib/acme/dns.thalheim.io/key.pem";
        tls-service-pem = "/var/lib/acme/dns.thalheim.io/fullchain.pem";
        https-port = "8053";
        http-notls-downstream = "yes";
        tls-port = 853;
        interface = [
          "127.0.0.1@53"
          "192.168.53.53@53"
          "127.0.0.1@8053"
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

  services.nginx = {
    virtualHosts."dns.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      http2 = true;
      locations."/".extraConfig = ''
        return 404 "404 Not Found\n";
      '';
      locations."/dns-query".extraConfig = ''
        grpc_pass grpc://127.0.0.1:8053;
      '';
    };
  };
}
