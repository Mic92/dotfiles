{
  programs.ssh.knownHosts.ssh-ca = {
    certAuthority = true;
    extraHostNames = [
      "*.r"
      "*.i"
      "*.thalheim.io"
      "*.dse.in.tum.de"
      "*.dos.cit.tum.de"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };
  programs.ssh.extraConfig = ''
    Host *.dse.in.tum.de !login.dse.in.tum.de !sarah.dse.in.tum.de !donna.dse.in.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    Host *.dos.cit.tum.de !login.dos.cit.tum.de !sarah.dos.cit.tum.de !donna.dos.cit.tum.de
      ProxyJump tunnel@login.dse.in.tum.de
  '';
  programs.ssh.knownHosts."login.dse.in.tum.de" = {
    hostNames = [ "login.dse.in.tum.de" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOdlUylM9WIFfIYZDK8rjVYQzX+RYwIlLgsEh4j0pNx6";
  };
}
