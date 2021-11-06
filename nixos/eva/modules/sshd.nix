{
  imports = [ ../../modules/sshd.nix ];
  services.openssh.extraConfig = "HostCertificate ${./eva-cert.pub}";
  services.openssh.listenAddresses = [
    { addr = "10.243.29.185"; port = 22; }
    { addr = "[42:0:3c46:8a42:2b1:5ef8:7562:676a]"; port = 22; }
  ];
}
