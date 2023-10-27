{ config, lib, ... }:
{
  services.nixos-wiki.hostname = "nixos-wiki.thalheim.io";
  services.nixos-wiki.githubClientId = "Iv1.95ed182c83df1d22";

  services.nginx.virtualHosts.${config.services.mediawiki.nginx.hostName} = {
    enableACME = lib.mkForce false;
    useACMEHost = "thalheim.io";
  };
}
