{ pkgs, ... }:

let
  mta-sts-web.locations."=/.well-known/mta-sts.txt".alias = pkgs.writeText "mta-sts.txt" ''
    version: STSv1
    mode: testing
    mx: mail.thalheim.io
    max_age: 86400
  '';
in
{
  services.nginx.virtualHosts."thalheim.io" = mta-sts-web;
  services.nginx.virtualHosts."devkid.net" = mta-sts-web;
  services.nginx.virtualHosts."lekwati.com" = mta-sts-web;
}
