{ config, lib, pkgs, ... }: 

let
  sanCertificate = { domain, rsa ? false }: {
    domain = domain;
    postRun = "systemctl reload nginx.service";
    group = "nginx";
    keyType = if rsa then "rsa2048" else "ec384";
    dnsProvider = "rfc2136";
    extraDomainNames = [ "*.${domain}" ];
    credentialsFile = config.sops.secrets.lego-knot-credentials.path;
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

    ../../../modules/nginx.nix
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
    #services.nginx.package = with pkgs; nginxStable.override {
    #    perl = null;
    #    modules = [ nginxModules.auth-ldap ];
    #  };
    #};
    sops.secrets.lego-knot-credentials.owner = "acme";

    security.acme.certs = {
      "lekwati.com" = sanCertificate  { domain = "lekwati.com"; };
      "legacy-lekwati.com" = sanCertificate  { domain = "lekwati.com"; rsa = true; };
      "thalheim.io" = sanCertificate  { domain = "thalheim.io"; };
      "legacy-thalheim.io" = sanCertificate { domain = "thalheim.io"; rsa = true; };
      "devkid.net" = sanCertificate { domain = "devkid.net"; };
      "legacy-devkid.net" = sanCertificate { domain = "devkid.net"; rsa = true; };
    };
  };
}
