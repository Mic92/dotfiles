{
  imports = [ ../../../nixosModules/sshd/tor.nix ];

  networking.firewall.allowedTCPPorts = [ 22 ];
}
