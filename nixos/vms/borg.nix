{
  box =
    { config, pkgs, lib, ... }:
    {
      deployment.targetHost = "172.16.49.86";
      imports = [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix> ];
      networking.hostName = "borg";

      boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "virtio_blk" ];
      boot.initrd.supportedFilesystems = [ "xfs" ];
      boot.kernelParams = ["boot.shell_on_fail"];
      boot.loader.grub.enable = true;
      boot.loader.grub.version = 2;
      boot.loader.grub.device = "/dev/vda";

      fileSystems."/" = {
        device = "/dev/vda1";
        fsType = "xfs";
      };

      services.tor = {
        enable = true;
        extraConfig = ''
          SocksPort 0

          HiddenServiceNonAnonymousMode 1
          HiddenServiceSingleHopMode 1

          ExitNodes {de}
          NewCircuitPeriod 120
        '';
        hiddenServices."ssh".map = [ { port = 22; } ];
      };

      services.openssh.enable = true;
      users.extraUsers.root.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
      ];

      nix.maxJobs = lib.mkDefault 1;

      system.stateVersion = "17.09";
    };
}
