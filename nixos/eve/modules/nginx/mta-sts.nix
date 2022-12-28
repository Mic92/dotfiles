{ pkgs, ... }:
let
  mta-sts-web = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."=/.well-known/mta-sts.txt".alias = pkgs.writeText "mta-sts.txt" ''
      version: STSv1
      mode: enforce
      mx: mail.thalheim.io
      max_age: 86400
    '';
  };
in
{
  services.nginx.virtualHosts."mta-sts.thalheim.io" = mta-sts-web;
  services.nginx.virtualHosts."mta-sts.devkid.net" = mta-sts-web;
  services.nginx.virtualHosts."mta-sts.lekwati.com" = mta-sts-web;
}
