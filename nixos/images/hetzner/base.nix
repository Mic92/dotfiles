{
  imports = [
    ../base-config.nix
    ./zfs.nix
    ../../vms/eve/modules/network.nix
  ];

  networking.nameservers = [
    "213.133.98.98"
    "213.133.99.99"
    "213.133.100.100"
    "2a01:4f8:0:1::add:1010"
    "2a01:4f8:0:1::add:9999"
    "2a01:4f8:0:1::add:9898"
  ];
}
