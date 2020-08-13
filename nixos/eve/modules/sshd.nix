{ config, ... }: {
  imports = [ ../../modules/sshd.nix ];

  services.openssh = {
    extraConfig = ''
      HostCertificate ${./eve-cert.pub}
    '';
    listenAddresses = [
      { addr = "0.0.0.0"; port = 22; }
      { addr = "[::]"; port = 22; }
      { addr = "[2a01:4f9:2b:1605::2]"; port = 443; }
    ];
  };

  networking.firewall.allowedTCPPorts = [ 22 443 ];

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
}
