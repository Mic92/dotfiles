{
  imports = [ ../../modules/sshd.nix ];
  services.openssh.extraConfig = "HostCertificate ${./eva-cert.pub}";
}
