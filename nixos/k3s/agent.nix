{ config, lib, pkgs, ... }:
{
  imports = [ ./. ];

  sops.secrets.k3s-server-token = {};
  services.k3s.role = "agent";
  # copied from server from bill:/var/lib/rancher/k3s/server/node-token
  services.k3s.tokenFile = config.sops.secrets.k3s-server-token.path;
  services.k3s.serverAddr = "https://node0.NixOS-cluster2.Serverless-tum.emulab.net:6443";
}
