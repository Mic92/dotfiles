{
  imports = [
    ../../modules/sshd/tor.nix
  ];

  services.openssh = {
    listenAddresses = [
      {
        addr = "0.0.0.0";
        port = 22;
      }
      {
        addr = "[::]";
        port = 22;
      }
      {
        addr = "[2a01:4f9:2b:1605::2]";
        port = 443;
      }
    ];
  };

  networking.firewall.allowedTCPPorts = [ 22 443 ];
}
