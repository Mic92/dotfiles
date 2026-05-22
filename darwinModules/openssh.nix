{ config, ... }:
{

  users.users.joerg.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLMlGNda7bilB0+3aMeJSFcB17auBPV0WhW60WlGZsQRF50Z/OgIHAA0/8HaxPmpIOLHv8JO3dCsj+OY1iS4FNo= joerg@turingmachine"
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIEVSsc5mlP8aWiUVwWWM3gKlB5LHVpmKSifnDyox/BnVAAAABHNzaDo= yubikey1"
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBCsjXKHCkpQT4LhWIdT0vDM/E/3tw/4KHTQcdJhyqPSH0FnwC8mfP2N9oHYFa2isw538kArd5ZMo5DD1ujL5dLk= ssh@secretive.Joerg’s-Laptop.local"
  ];
  users.users.root.openssh.authorizedKeys.keys = config.users.users.joerg.openssh.authorizedKeys.keys;

  environment.etc."ssh/ssh_config.d/jumphost.conf".text = ''
    Host *.dse.in.tum.de !login.dse.in.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    Host *.dos.cit.tum.de !login.dos.cit.tum.de
      ProxyJump tunnel@login.dse.in.tum.de

    # The TUM jumphost presents an ITO-signed certificate on its ed25519 host
    # key, but our cluster CA only signed its ecdsa/rsa host keys. Prefer those
    # so host verification goes through our CA.
    Host login.dse.in.tum.de login.dos.cit.tum.de
      HostKeyAlgorithms ecdsa-sha2-nistp256-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com

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

  # CA generated on the new TUM jumphost VM (dosvm3); its host certificate
  # only lists dosvm3.cit.tum.de as principal, so it cannot be used for the
  # login.* aliases yet.
  programs.ssh.knownHosts.tum-jumphost-ca = {
    certAuthority = true;
    hostNames = [ "dosvm3.cit.tum.de" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH9d9mR3PHGWKa77BfvbqmX3Y6pI/i/X/tcpWCms26EY";
  };
}
