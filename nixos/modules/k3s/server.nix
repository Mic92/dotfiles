{ config, lib, pkgs, ... }:
{
  imports = [ ./. ];

  #sops.secrets.k3s-server-token = {};
  #systemd.tmpfiles.rules = [
  #  "L+ /var/lib/rancher/k3s/server/token - - - - ${config.sops.secrets.k3s-server-token.path}"
  #];
  services.k3s.extraFlags = "--flannel-backend=host-gw --container-runtime-endpoint unix:///run/containerd/containerd.sock";
}
