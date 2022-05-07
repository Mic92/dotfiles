{
  imports = [
    ../../modules/tor-ssh.nix
  ];

  services.openssh.extraConfig = ''
    HostCertificate ${./rock-cert.pub}
  '';
}
