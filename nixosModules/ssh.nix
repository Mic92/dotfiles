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

    # The TUM jumphost presents an ITO-signed certificate on its ed25519 host
    # key, but our cluster CA only signed its ecdsa/rsa host keys. Prefer those
    # so host verification goes through our CA.
    Host login.dse.in.tum.de login.dos.cit.tum.de
      HostKeyAlgorithms ecdsa-sha2-nistp256-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com

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
  # CA generated on the new TUM jumphost VM (dosvm3); its host certificate
  # only lists dosvm3.cit.tum.de as principal, so it cannot be used for the
  # login.* aliases yet.
  programs.ssh.knownHosts.tum-jumphost-ca = {
    certAuthority = true;
    hostNames = [ "dosvm3.cit.tum.de" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH9d9mR3PHGWKa77BfvbqmX3Y6pI/i/X/tcpWCms26EY";
  };
}
