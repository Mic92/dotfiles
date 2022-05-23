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
      "*.dse.in.tum.de"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };
  programs.ssh.extraConfig = ''
    Host *.dse.in.tum.de !login.dse.in.tum.de
      ProxyJump tunnel@login.dse.in.tum.de
 '';
}
