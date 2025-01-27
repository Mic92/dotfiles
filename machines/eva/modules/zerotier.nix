{
  imports = [ ../../../nixosModules/zerotier.nix ];
  clan.zerotier.moon.stableEndpoints = [
    "89.58.27.144"
    "2a03:4000:62:fdb::"
  ];
  services.zerotierone.blockRfc1918Addresses = true;
}
