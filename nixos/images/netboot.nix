{ pkgs ? import <nixpkgs> {} }:
let
  bootSystem = import <nixpkgs/nixos> {
    configuration = { ... }: {
      imports = [
        <nixpkgs/nixos/modules/installer/netboot/netboot-minimal.nix>
        ./base-config.nix
        ./zfs.nix
      ];
    };
  };
in pkgs.symlinkJoin {
  name = "netboot";
  paths = with bootSystem.config.system.build; [
    netbootRamdisk
    kernel
    netbootIpxeScript
  ];
  preferLocalBuild = true;
}

# n=$(realpath /tmp/netboot)
# init=$(grep -ohP 'init=\S+' $n/netboot.ipxe)
# nix build -o /tmp/pixiecore nixpkgs.pixiecore
