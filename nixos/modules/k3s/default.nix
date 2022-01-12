{ lib, config, ... }:
{
  services.k3s.enable = true;
  services.k3s.docker = lib.mkForce false;

  # We only want k3s to manage firewalls or else we have to duplicate firewall
  # management.
  networking.firewall.enable = false;

  sops.secrets.k3s-server-token = {};
  services.k3s.tokenFile = config.sops.secrets.k3s-server-token.path;
}
