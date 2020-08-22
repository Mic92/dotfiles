{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/tor-ssh.nix
  ];
  services.openssh.extraConfig = ''
    HostCertificate ${./eddie-cert.pub}
  '';
}
