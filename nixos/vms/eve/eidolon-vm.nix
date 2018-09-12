## USAGE
# $ nix-build kexec-installer.nix
# can be deployed remote like this
# $ rsync -aL -e ssh result/ <host>:
## Customize it like this
# # custom-installer.nix
# (import ./kexec-installer.nix) {
#   extraConfig = {pkgs, ... } {
#     user.extraUsers.root.openssh.authorizedKeys.keys = [ "<your-key>" ];
#     services.openssh = {
#        enable = true;
#        startWhenNeeded = true;
#     }
#   }
# }
# $ nix-build custom-installer.nix
# $ ls -la ./result
let
  config = (import <nixpkgs/nixos/lib/eval-config.nix> {
    modules = [
      <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
      <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
      <nixpkgs/nixos/modules/virtualisation/grow-partition.nix>
      ({config, pkgs, lib, ... }: {

        networking.usePredictableInterfaceNames = false;

        environment.systemPackages = with pkgs; [
          wget
          tmux
          iproute
          ethtool
          tcpdump
          netcat
          iperf
        ];

        networking.firewall.enable = lib.mkOverride 150 false;
        services.mingetty.autologinUser = "root";
        services.postgresql = {
          enable = true;
          authentication = "local all all ident";
        };

        systemd.services = {
          "serial-getty@ttyS0".wantedBy = [ "multi-user.target" ];
        };

        boot.loader.grub = {
          version = 2;
          device = "/dev/sda";
        };

        services.openssh = {
          enable = true;
          permitRootLogin = "prohibit-password";
        };

        users.extraUsers.root.openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ8SrUbfV2dVgKnqza9RqPsalV//OmIk26wEdibUseHn nek0"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
        ];
        networking.hostName = "eidolon";

        virtualisation.growPartition = true;
        fileSystems."/" = {
          device = "/dev/disk/by-label/nixos";
          autoResize = true;
        };
      })
    ];
  }).config;
  pkgs = import <nixpkgs> {};
in import <nixpkgs/nixos/lib/make-disk-image.nix> {
    inherit config pkgs;
    inherit (pkgs) lib;
    partitioned = true;
    diskSize = 2048;
    format = "qcow2";
    #configFile = pkgs.writeText "configuration.nix"
    #  ''
    #    {
    #      imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
    #      ${optionalString config.ec2.hvm ''
    #        ec2.hvm = true;
    #      ''}
    #    }
    #  '';
  }
