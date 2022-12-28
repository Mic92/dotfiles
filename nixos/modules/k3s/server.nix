{
  imports = [ ./. ];

  services.k3s.extraFlags = "--flannel-backend=host-gw --container-runtime-endpoint unix:///run/containerd/containerd.sock";
}
