# $ nix-build iso.nix
# $ ls -la result/iso

with (import <nixpkgs> {});

let
  sshKeys = (import ./ssh-keys.nix);

  module = {pkgs, ...}: {
    environment.systemPackages = [ pkgs.vim ];
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
    };
    users.extraUsers.root.openssh.authorizedKeys.keys = with sshKeys; alfred ++ joerg;
    networking = {
      firewall.allowedTCPPorts = [ 22 ];
      usePredictableInterfaceNames = false;
      useDHCP = false;
    };
    systemd.network.enable = true;
    environment.etc."systemd/network/eth0.network".text = ''
      [Match]
      Name = eth0

      [Network]
      Address = 64.137.201.46/24
      Gateway = 64.137.201.1
    '';
  };

  config = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = builtins.currentSystem;
    modules = [
      <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
      module
    ];
  }).config;

  ipxeScript = pkgs.writeText "nixos.ixpe" ''
    #!ipxe
    ifopen net0
    set net0/ip 192.168.0.2
    set net0/netmask 255.255.255.0
    set net0/gateway 192.168.0.1
    set net0/dns 8.8.8.8

    kernel https://nixboot.higgsboson.tk/bzImage init=${config.system.build.toplevel}/init root=/rootfs boot.shell_on_fail nomodeset loglevel=7 initrd=initrd.gz
    initrd https://nixboot.higgsboson.tk/initrd.gz

    boot
  '';

  download = ''
    if [ ! -f ./bzImage ]; then
      wget -O ./bzImage https://nixboot.higgsboson.tk/bzImage
    fi
    if [ ! -f ./initrd.gz ]; then
      wget -O ./initrd.gz https://nixboot.higgsboson.tk/initrd.gz
    fi
    if [ ! -f ./sha256sum ]; then
      wget -O sha256sum https://nixboot.higgsboson.tk/sha256sum
    fi
    sha256sum -c < sha256sum
  '';

  qemuScript = pkgs.writeScript "qemu-run" ''
    #!/bin/sh
    ${download}
    exec qemu-system-x86_64 \
      -m 1024 \
      -kernel ./bzImage \
      -initrd ./initrd.gz \
      -append "console=ttyS0 init=${config.system.build.toplevel}/init root=/rootfs boot.shell_on_fail nomodeset loglevel=7" \
      -nographic
  '';

  kexecScript = pkgs.writeScript "kexec-run" ''
    #!/bin/sh
    ${download}
    kexec \
      --initrd=./initrd.gz \
      --command-line "console=ttyS0 init=${config.system.build.toplevel}/init root=/rootfs boot.shell_on_fail nomodeset loglevel=7" \
      ./bzImage
  '';

  grubConfig = pkgs.writeText "grub-entry.cfg" ''
    menuentry 'Linux NetBoot Environment' {
      set root='(hd0,1)'
      linux16 /boot/ipxe.lkrn
      initrd16 /boot/nixos.ipxe
    }
  '';

in with config.system.build;

  stdenv.mkDerivation rec {
  name = "netboot";
  phases = ["installPhase"];
  src = ./dummy;
  installPhase = ''
    mkdir -p $out
    (
      cd $(mktemp -d)
      zcat ${initialRamdisk}/initrd | ${pkgs.cpio}/bin/cpio -i

      # pseudo rootfs
      ln -s /nix-store.squashfs nix-store.squashfs
      ${pkgs.squashfsTools}/bin/mksquashfs nix-store.squashfs rootfs
      rm nix-store.squashfs

      cp ${squashfsStore} nix-store.squashfs

      find . | ${pkgs.cpio}/bin/cpio --create --format='newc' | gzip > $out/initrd.gz
    )
    cp ${kernel}/bzImage $out/bzImage
    sha256sum $out/{bzImage,initrd.gz} > $out/sha256sum
    cp ${pkgs.ipxe}/* $out/
    cp ${ipxeScript} $out/nixos.ipxe
    install -m755 ${qemuScript} $out/qemu-run
    install -m755 ${kexecScript} $out/kexec-run

    #ln -s ${grubConfig} grub-entry
  '';
}
