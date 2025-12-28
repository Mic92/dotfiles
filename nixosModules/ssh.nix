{
  # extra host names for the ssh-ca defined in clan
  programs.ssh.knownHosts.ssh-ca.extraHostNames = [
    "*.dse.in.tum.de"
    "*.dos.cit.tum.de"
  ];
  programs.ssh.extraConfig = ''
    Host *.dse.in.tum.de !login.dse.in.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    Host *.dos.cit.tum.de !login.dos.cit.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    # Ghaf T14 Gen 5
    Host ghaf-netvm
      HostName 192.168.188.25
      User root
      StrictHostKeyChecking no

    Host ghaf-host
      HostName ghaf-host
      User root
      ProxyJump ghaf-netvm
      StrictHostKeyChecking no

    Host ghaf-gui
      HostName gui-vm
      User root
      ProxyJump ghaf-netvm
      StrictHostKeyChecking no
  '';
  programs.ssh.knownHosts."login.dse.in.tum.de" = {
    hostNames = [ "login.dse.in.tum.de" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOdlUylM9WIFfIYZDK8rjVYQzX+RYwIlLgsEh4j0pNx6";
  };
}
