# configuration.nix
# add
# $ nixos-generate-config
# imports = [ ((builtins.fetchGit { url = "https://github.com/Mic92/dotfiles" }) + "/nixos/images/hetzner/bootstrap.nix") ];
# $ nix-shell -p git --command 'nixos-install'
{ ... }: {
  imports = [
    ./hetzner-base.nix
  ];

  initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 2222;
      hostECDSAKey = "/run/keys/initrd-ssh-key";
    };
    postCommands = ''
      echo "cryptsetup-askpass" >> /root/.profile

      ip addr add 95.216.112.61/26 dev eth0
      ip route add default via 95.216.112.1
      ip addr add 2a01:4f9:2b:1605::2/64
      ip route add default via fe80::1 dev eth0
    '';
  };

  boot.loader.grub.devices = ["/dev/sda" "/dev/sdb"];
}
