{config, ...}: {
  imports = [
    ./acme.nix
  ];

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    commonHttpConfig = ''
      add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains; preload' always;
    '';

    resolver.addresses =
      if config.networking.nameservers == []
      then ["1.1.1.1"]
      else config.networking.nameservers;

    sslDhparam = config.security.dhparams.params.nginx.path;
  };

  security.dhparams = {
    enable = true;
    params.nginx = {};
  };
  networking.firewall.allowedTCPPorts = [80 443];
}
