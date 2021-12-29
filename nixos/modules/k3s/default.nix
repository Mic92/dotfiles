{ lib, config, ... }:
{
  services.k3s.enable = true;
  services.k3s.docker = lib.mkForce false;
  sops.secrets.k3s-server-token = {};
  services.k3s.tokenFile = config.sops.secrets.k3s-server-token.path;
}
