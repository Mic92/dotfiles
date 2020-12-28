{ config, pkgs, ... }: {
  virtualisation.docker.storageDriver = "zfs";

  services.kubernetes = {
    addons.dashboard.enable = true;
    masterAddress = "eve.r";
    proxy.hostname = "eve.thalheim.io";

    apiserver = {
      securePort = 8443;
      advertiseAddress = "eve.r";
    };

    addons.dns.enable = true;

    easyCerts = true;
    roles = [ "master" "node" ];
  };

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
