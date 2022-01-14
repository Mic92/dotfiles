{ config, lib, pkgs, ... }:
{
  imports = [ ./. ];

  services.k3s.role = "agent";
  services.k3s.serverAddr = "https://node0.nixos.Serverless-tum.emulab.net:6443";
  services.k3s.extraFlags = "--container-runtime-endpoint unix:///run/containerd/containerd.sock";
}
