{ config, pkgs, ... }: {
  virtualisation.docker.storageDriver = "zfs";

  #services.kubernetes = {
  #  addons.dashboard.enable = true;
  #  proxy.hostname = "eve.thalheim.io";

  #  easyCerts = true;
  #  roles = [ "master" "node" ];
  #  masterAddress = "eve.r";
  #};
  environment.systemPackages = [
    (pkgs.runCommand "wrap-kubectl" {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
      mkdir -p $out/bin
      makeWrapper ${pkgs.kubernetes}/bin/kubectl $out/bin/kubectl \
        --set KUBECONFIG "/etc/kubernetes/cluster-admin.kubeconfig"
    '')
  ];

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
    6443
  ];
}
