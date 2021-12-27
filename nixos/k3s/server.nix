{ config, lib, pkgs, ... }:
{
  imports = [ ./. ];
  # k3s api server
  sops.secrets.telegraf-github-token.owner = "telegraf";
  networking.firewall.allowedTCPPorts = [
    6443
  ];
  services.k3s.extraFlags = "--flannel-backend=host-gw";
}
