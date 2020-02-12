{ config, ... }: {
  imports = [ ../../modules/sshd.nix ];

  services.openssh.listenAddresses = [
    { addr = "0.0.0.0"; port = 22; }
    { addr = "0.0.0.0"; port = 22022; } # legacy
    { addr = "[::]"; port = 22; }
    { addr = "[::]"; port = 22022; } # legacy
    { addr = "[2a01:4f9:2b:1605::2]"; port = 443; }
  ];

  networking.firewall.allowedTCPPorts = [ 22 22022 443 ];

  services.openldap.extraConfig = ''
    attributetype ( 1.3.6.1.4.1.24552.500.1.1.1.13 NAME 'sshPublicKey'
       DESC 'MANDATORY: OpenSSH Public key'
       EQUALITY octetStringMatch
       SYNTAX 1.3.6.1.4.1.1466.115.121.1.40 )
    # printableString SYNTAX yes|no
    objectclass ( 1.3.6.1.4.1.24552.500.1.1.2.0 NAME 'ldapPublicKey' SUP top AUXILIARY
       DESC 'MANDATORY: OpenSSH LPK objectclass'
       MUST ( sshPublicKey $ uid )
       )
  '';

  services.icinga2.extraConfig = ''
    apply Service "SSH v4 (eve)" {
      import "eve-service"
      check_command = "ssh"
      vars.ssh_ipv4 = true
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "SSH v6 (eve)" {
      import "eve-service"
      check_command = "ssh"
      vars.ssh_ipv6 = true
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
