{
  imports = [ ../../../nixosModules/zerotier.nix ];
  clan.zerotier.moon.stableEndpoints = [
    "116.203.179.132"
    "2a01:4f8:1c1a:37b2::1"
  ];
  services.zerotierone.blockRfc1918Addresses = true;
}
