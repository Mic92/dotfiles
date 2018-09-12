{pkgs, conf, lib, ...}:

{
  services.unbound = {
    enable = true;
    extraConfig = ''
      ssl-service-key
      ssl-service-pem
    '';
  };
}
