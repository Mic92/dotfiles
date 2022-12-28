{
  imports = [ ./. ];

  services.k3s.role = "agent";
  services.k3s.serverAddr = "https://node0.nixos-k3s.Serverless-tum.emulab.net:6443";
  services.k3s.extraFlags = "--container-runtime-endpoint unix:///run/containerd/containerd.sock";
}
