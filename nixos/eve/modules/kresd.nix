{ pkgs, config, ... }: {
  services.kresd = {
    enable = true;
    # proxied with nixos/eve/modules/sslh.nix
    listenDoH = [
      "[::1]:5343"
      "127.0.0.1:5343"
    ];
    listenTLS = [
      "0.0.0.0:853"
      "[::]:853"
    ];
    listenPlain = [
      "[::1]:53"
      "127.0.0.1:53"
    ];
    extraConfig = ''
      net.tls("/var/lib/acme/dns.thalheim.io/fullchain.pem", "/var/lib/acme/dns.thalheim.io/key.pem")
      modules = { 'hints > iterate' }
      hints.add_hosts('${pkgs.retiolum}/etc.hosts')
    '';
  };

  nixpkgs.config.packageOverrides = pkgs: {
    knot-resolver = pkgs.knot-resolver.override { extraFeatures = true; };
  };

  # This causes services to fail on upgrade
  systemd.services."kresd@".restartIfChanged = false;

  # dns.thalheim.io
  networking.firewall.allowedTCPPorts = [ 853 ];

  security.acme.certs."dns.thalheim.io" = {
    postRun = "systemctl restart kresd@1.service";
    group = "knot-resolver";
    dnsProvider = "rfc2136";
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
  };
}
