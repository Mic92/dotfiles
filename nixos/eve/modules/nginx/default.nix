{ config, lib, pkgs, ... }: 

let
  sanCertificate = { domain, rsa ? false }: let
    wantedVhosts = lib.filterAttrs (_: attrs: (attrs.useACMEHost or null) == domain)
      config.services.nginx.virtualHosts;
    serverAliases = lib.flatten (lib.mapAttrsToList (_: vhost: vhost.serverAliases) wantedVhosts);
  in {
    domain = domain;
    extraDomains = (
      lib.mapAttrs (name: _: null) wantedVhosts
    ) // (lib.foldl (domains: domain: domains // { ${domain} = null; }) {} serverAliases);
    postRun = "systemctl reload nginx.service";
    webroot = "/var/lib/acme/acme-challenge";
    allowKeysForGroup = true;
    group = "nginx";
    keyType = if rsa then "rsa2048" else "ec384";
  };
in {
  imports = [
    ./blog.nix
    ./devkid.net.nix
    ./dl.nix
    ./glowing-bear.nix
    ./homepage.nix
    ./ip.nix
    ./muc.nix
    #./blog.halfco.de.nix
    #./halfco.de.nix
    ./stub-status.nix
    ./threema.nix
    ./retiolum.nix
  ];

  # avoid conflict with sslh by binding to port 4443
  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      config.listen = lib.mkDefault [
        { addr = "127.0.0.1"; port = 4443; ssl = true;}
        { addr = "[2a01:4f9:2b:1605::1]"; port = 443; ssl = true;}
        { addr = "0.0.0.0"; port = 80; ssl = false;}
        { addr = "[2a01:4f9:2b:1605::1]"; port = 80; ssl = false;}
      ];
    });
  };

  config = {
    services.nginx = {
      package = pkgs.nginx.override {
        modules = with pkgs.nginxModules; [ ldap-auth ];
      };
      enable = true;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      commonHttpConfig = ''
        add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains; preload' always;
      '';

      resolver.addresses = ["127.0.0.1"];

      sslDhparam = config.security.dhparams.params.nginx.path;
    };

    security.dhparams = {
      enable = true;
      params.nginx = {};
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    #security.acme.production = false;
    security.acme.email = "joerg.letsencrypt@thalheim.io";
    security.acme.acceptTerms = true;
    security.acme.certs = {
      "thalheim.io" = sanCertificate  { domain = "thalheim.io"; };
      "legacy-thalheim.io" = sanCertificate { domain = "thalheim.io"; rsa = true; };
      "devkid.net" = sanCertificate { domain = "devkid.net"; };
      "legacy-devkid.net" = sanCertificate { domain = "devkid.net"; rsa = true; };
      "higgsboson.tk" = sanCertificate { domain = "higgsboson.tk"; };
      "legacy-higgsboson.tk" = sanCertificate { domain = "higgsboson.tk"; rsa = true; };
    };

    environment.etc."netdata/python.d/web_log.conf".text = ''
      nginx_log3:
        name: 'nginx'
        path: '/var/log/nginx/access.log'
    '';

    users.users.netdata.extraGroups = [ "nginx" ];
    services.openldap.extraConfig = ''
      objectClass ( 1.3.6.1.4.1.28295.1.2.4 NAME 'nginx'
              SUP top AUXILIARY
              DESC 'Added to an account to allow nginx access'
      	MUST ( mail $ userPassword ))
    '';
  };
}
