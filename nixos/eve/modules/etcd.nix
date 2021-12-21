{
  imports = [
    ../../modules/etcd.nix
  ];
  services.etcd.initialClusterState = "new";
}
