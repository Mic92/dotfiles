{ pkgs, ... }: {
  virtualisation.docker.storageDriver = "zfs";

  services.kubernetes = {
    masterAddress = "eve.r";
    kubeconfig.server = "https://10.243.29.174:8443";
    proxy.hostname = "eve.r";
    kubelet.nodeIp = "10.243.29.174";

    apiserverAddress = "https://10.243.29.174:8443";
    apiserver.advertiseAddress = "10.243.29.174";

    apiserver.securePort = 8443;

    easyCerts = true;
    roles = [ "master" "node" ];
  };

  environment.systemPackages = [
    (pkgs.runCommand "wrap-kubectl"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
      } ''
      mkdir -p $out/bin
      makeWrapper ${pkgs.kubernetes}/bin/kubectl $out/bin/kubectl \
        --set KUBECONFIG "/etc/kubernetes/cluster-admin.kubeconfig"
    '')
  ];

  networking.firewall.allowedTCPPorts = [
    8443
  ];
  #networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [
  #];
}
