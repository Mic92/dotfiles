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

    # FIXME:
    # nginx: [alert] could not open error log file: open() "/var/log/nginx/error.log" failed (2: No such file or directory)
    # 2022/12/23 02:57:47 [emerg] 7#7: BIO_new_file("/var/lib/dhparams/nginx.pem") failed (SSL: error:80000002:system library::No such file or directory:calling fopen(/var/lib/dhparams/nginx.pem, r) error:10000080:BIO routines::no such file)
    validateConfig = false;
  };

  security.dhparams = {
    enable = true;
    params.nginx = {};
  };
  networking.firewall.allowedTCPPorts = [80 443];
}
