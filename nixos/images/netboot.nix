{
  pkgs,
  nixosSystem,
  extraModules ? [],
}: let
  bootSystem = nixosSystem {
    system = pkgs.system;
    modules = [
      ./base-config.nix
      ./zfs.nix
      ({modulesPath, ...}: {
        imports =
          [
            (modulesPath + "/installer/netboot/netboot-minimal.nix")
          ]
          ++ extraModules;

        # IPMI SOL console redirection stuff
        boot.kernelParams = [
          "console=ttyS0,115200n8"
          "console=ttyAMA0,115200n8"
          "console=tty0"
        ];
      })
    ];
  };
in
  pkgs.symlinkJoin {
    name = "netboot";
    paths = with bootSystem.config.system.build; [
      netbootRamdisk
      kernel
      netbootIpxeScript
    ];
    preferLocalBuild = true;
  }
# nix build -o /tmp/pixiecore '.#netboot'
# n=$(realpath /tmp/netboot)
# init=$(grep -ohP 'init=\S+' $n/netboot.ipxe)

