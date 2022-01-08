{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/tor-ssh.nix
  ];

  services.openssh.extraConfig = ''
    HostCertificate ${./jarvis-cert.pub}
  '';
}
