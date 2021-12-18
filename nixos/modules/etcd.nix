{ config, ... }: {
  services.etcd = {
    enable = true;
    clientCertAuth = true;
    certFile = config.sops.secrets.etcd-cert.path;
    keyFile = config.sops.secrets.etcd-key.path;
    peerKeyFile = config.sops.secrets.etcd-peer-cert.path;
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
    initialAdvertisePeerUrls = [
      "http://${config.networking.hostName}.etcd.thalheim.io:2379"
    ];
    advertiseClientUrls = [
      "http://${config.networking.hostName}.etcd.thalheim.io:2379"
    ];
    extraConf.ETCD_DISCOVERY_SRV = "thalheim.io";
  };
  sops.secrets.etcd-key = { };
  sops.secrets.etcd-cert = { };
  sops.secrets.etcd-peer-cert = { };
  sops.secrets.etcd-peer-key = { };
}
