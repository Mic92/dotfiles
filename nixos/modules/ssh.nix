{ config, lib, pkgs, ... }:
{
  programs.ssh.knownHosts.ssh-ca = {
    certAuthority = true;
    hostNames = [
      "*.r"
      "*.i"
      "*.thalheim.io"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };
}
