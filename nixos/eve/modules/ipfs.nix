{
  services.ipfs.enable = true;
  services.ipfs.gatewayAddress = "/ip4/0.0.0.0/tcp/8080";
  networking.firewall.allowedTCPPorts = [
    4001 5001 8080
  ];
}
