{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/tor-ssh.nix
  ];

  services.openssh.extraConfig = ''
    HostCertificate ${./turingmachine-cert.pub}
  '';
}
