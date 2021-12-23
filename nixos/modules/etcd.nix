{ config, pkgs, lib, ... }: {
  services.etcd = {
    enable = true;
    clientCertAuth = true;
    certFile = config.sops.secrets.etcd-cert.path;
    keyFile = config.sops.secrets.etcd-key.path;
    peerKeyFile = config.sops.secrets.etcd-peer-key.path;
    peerCertFile = config.sops.secrets.etcd-peer-cert.path;
    peerTrustedCaFile = builtins.toFile "etc-ca.pem" ''
      -----BEGIN CERTIFICATE-----
      MIICTzCCAdWgAwIBAgIUdo4C2x/cgcv/H+Lt3rkrpuEACEowCgYIKoZIzj0EAwMw
      ZzELMAkGA1UEBhMCREUxETAPBgNVBAgTCFRoYWxoZWltMQswCQYDVQQHEwJTQTER
      MA8GA1UEChMIVGhhbGhlaW0xDTALBgNVBAsTBENvbmYxFjAUBgNVBAMTDVRoYWxo
      ZWltIEVUQ0QwHhcNMjExMjE3MjA0OTAwWhcNMjYxMjE2MjA0OTAwWjBnMQswCQYD
      VQQGEwJERTERMA8GA1UECBMIVGhhbGhlaW0xCzAJBgNVBAcTAlNBMREwDwYDVQQK
      EwhUaGFsaGVpbTENMAsGA1UECxMEQ29uZjEWMBQGA1UEAxMNVGhhbGhlaW0gRVRD
      RDB2MBAGByqGSM49AgEGBSuBBAAiA2IABOzb240ayT+fXZn9QvmuxW7J+A2toNIl
      nZAtvfi6l4epaSOYch5Cs4h3s2pN8Ha7+OuGP6wXJJCEetk5wDS1CLIKjnapczpU
      80c/EGmlDpoD30hyGw0Jh3/WTARrCl3wR6NCMEAwDgYDVR0PAQH/BAQDAgEGMA8G
      A1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFN0I+VBB8i8Zz2nX2g23IkT+r8THMAoG
      CCqGSM49BAMDA2gAMGUCMEY3iRdOjMvZOzulihIfsuYtgdeKIlWAuvy2fvbW2sfG
      ad7RafGpIX7sELNv3SzkbwIxAI+oHHuKLeaaK0MTnvbrc86tQxRJ/8SgLjcNhrtu
      Vfpx1yTjrZyoY4lUD8Sp8kUzTg==
      -----END CERTIFICATE-----
    '';
    trustedCaFile = config.services.etcd.peerTrustedCaFile;
    listenClientUrls = [
      "https://[::]:2379"
    ];
    listenPeerUrls = [
      "https://[::]:2380"
    ];
    name = "${config.networking.hostName}.etcd.thalheim.io";
    initialCluster = [];
    initialAdvertisePeerUrls = [
      "https://eva.etcd.thalheim.io:2380"
      "https://eve.etcd.thalheim.io:2380"
      #"https://rose.etcd.thalheim.io:2380"
    ];
    advertiseClientUrls = [
      "https://${config.networking.hostName}.etcd.thalheim.io:2379"
    ];
    #initialAdvertisePeerUrls = [
    #  "https://${config.networking.hostName}.etcd.thalheim.io:2380"
    #];
    initialClusterState = lib.mkDefault "new";
    extraConf.DISCOVERY_SRV = "thalheim.io";
  };
  networking.firewall.interfaces."retiolum".allowedTCPPorts = [
    2379 2380
  ];
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "etcdctl" ''
      exec ${pkgs.etcd}/bin/etcdctl \
        --endpoint https://${config.networking.hostName}.etcd.thalheim.io:2379 \
        --ca-file=${config.services.etcd.peerTrustedCaFile} \
        --cert-file=${config.sops.secrets.etcd-admin-cert.path} \
        --key-file=${config.sops.secrets.etcd-admin-key.path} \
        "$@"
    '')
  ];
  sops.secrets.etcd-key.owner = "etcd";
  sops.secrets.etcd-cert.owner = "etcd";
  sops.secrets.etcd-peer-cert.owner = "etcd";
  sops.secrets.etcd-peer-key.owner = "etcd";
  sops.secrets.etcd-admin-cert.sopsFile = ./etcd-admin.yaml;
  sops.secrets.etcd-admin-key.sopsFile = ./etcd-admin.yaml;
}
