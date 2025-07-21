{
  imports = [ ../../../nixosModules/zerotier.nix ];

  services.zerotierone.blockRfc1918Addresses = true;
}
