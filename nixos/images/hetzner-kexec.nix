# SSD1: 1MB Grub, 2G ext4/kernel + nixos rescue kexec, rest zfs
# $ sgdisk  -n 1:0:+1M -n 2:0:+2G -N 3 -t 1:ef02 -t 2:8304 -t 3:8304 /dev/sda
# SSD2: zfs
# $ sgdisk  -n 1:0:+1M -n 2:0:+2G -N 3 -t 1:ef02 -t 2:8304 -t 3:8304 /dev/sda
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.kexec_tarball -I nixos-config=./kexec.nix nixcfg=./hetzner-kexec.nix
# $ mkfs.vfat -b32 -n bios-boot /dev/sda1
# $ mkfs.ext4 -L linux-boot /dev/sda2
# $ mount /dev/sda2 /mnt
# $ rsync -e ssh -a result/tarball/nixos-system-x86_64-linux.tar.xz root@95.216.112.61:/mnt
# rescue> tar -C / -xf nixos-system-x86_64-linux.tar.xz
# rescue> /kexec_nixos
# $ zpool create -o ashift=12 -o altroot=/mnt -O acltype=posixacl -O xattr=sa -O compression=lz4 zroot /dev/sda3
# $ zfs create -o encryption=aes-256-gcm -o keyformat=passphrase -o mountpoint=none zroot/root

# $ zfs create -o mountpoint=legacy -o sync=disabled zroot/root/tmp
# $ zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/home
# $ zfs create -o mountpoint=legacy -o com.sun:auto-snapshot=true zroot/root/nixos
# $ mount -t zfs zroot/root/nixos /mnt
# $ mkdir /mnt/{home,tmp,boot}
# $ mount /dev/sda2 /mnt/boot/
# $ mount -t zfs zroot/root/home /mnt/home/
# $ mount -t zfs zroot/root/tmp /mnt/tmp/
# $ nixos-generate-config

# continue with hetzner-bootstrap.nix
{...}: {
  networking.dhcpcd.enable = false;

  imports = [
    ./zfs.nix
  ];
  systemd.network = {
    enable = true;
    networks."eth0".extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      Address = 95.216.112.61/26
      Gateway = 95.216.112.1
      Address = 2a01:4f9:2b:1605::2/64
      Gateway = fe80::1
      IPv6AcceptRA = no
      IPForward = yes

      [DHCP]
      UseDNS = no
    '';
  };

  networking.nameservers = [
    "213.133.98.98"
    "213.133.99.99"
    "213.133.100.100"
    "2a01:4f8:0:1::add:1010"
    "2a01:4f8:0:1::add:9999"
    "2a01:4f8:0:1::add:9898"
  ];
}
