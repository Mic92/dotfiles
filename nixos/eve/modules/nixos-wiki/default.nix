{ config, lib, ... }:
{
  services.nixos-wiki = {
    hostname = "nixos-wiki.thalheim.io";
    adminPasswordFile = config.sops.secrets.nixos-wiki.path;
    githubClientId = "Iv1.95ed182c83df1d22";
    githubClientSecretFile = config.sops.secrets.nixos-wiki-github-client-secret.path;
    emergencyContact = "nixos-wiki@thalheim.io";
    passwordSender = "nixos-wiki@thalheim.io";
    noReplyAddress = "nixos-wiki-no-reply@thalheim.io";
  };

  services.nginx.virtualHosts.${config.services.mediawiki.nginx.hostName} = {
    enableACME = lib.mkForce false;
    useACMEHost = "thalheim.io";
  };
}
