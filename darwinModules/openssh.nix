{
  users.users.joerg.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE"
  ];

  environment.etc."ssh/ssh_config.d/jumphost.conf".text = ''
    Host *.dse.in.tum.de !login.dse.in.tum.de !sarah.dse.in.tum.de !donna.dse.in.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    Host *.dos.cit.tum.de !login.dos.cit.tum.de !sarah.dos.cit.tum.de !donna.dos.cit.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    Host *.vpn.clan.lol
      ProxyJump tunnel@clan.lol

    Host storinator01
      ProxyJump tunnel@clan.lol
      Hostname fda9:b487:2919:3547:3699:9393:7f57:6e6b

    Host build02
      ProxyJump tunnel@clan.lol
      Hostname 100.98.54.8
  '';

  programs.ssh.knownHosts.ssh-ca = {
    certAuthority = true;
    hostNames = [
      "*.r"
      "*.i"
      "*.thalheim.io"
      "*.dse.in.tum.de"
      "*.dos.cit.tum.de"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };

  programs.ssh.knownHosts."login.dse.in.tum.de" = {
    hostNames = [ "login.dse.in.tum.de" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOdlUylM9WIFfIYZDK8rjVYQzX+RYwIlLgsEh4j0pNx6";
  };
}
