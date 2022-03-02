{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.ssh.knownHosts.ssh-ca = {
    certAuthority = true;
    extraHostNames = [
      "*.r"
      "*.i"
      "*.thalheim.io"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };
}
