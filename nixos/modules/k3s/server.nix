{ config, lib, pkgs, ... }:
{
  imports = [ ./. ];
  # k3s api server
  networking.firewall.allowedTCPPorts = [
    6443
  ];

  sops.secrets.k3s-server-token = {};
  systemd.tmpfiles.rules = [
    "L+ /var/lib/rancher/k3s/server/token - - - - ${config.sops.secrets.k3s-server-token.path}"
  ];
  services.k3s.extraFlags = "--flannel-backend=host-gw";
}
