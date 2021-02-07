{
  imports = [ ../../modules/sshd.nix ];
  services.openssh.extraConfig = "HostCertificate ${./bernie-cert.pub}";
}
