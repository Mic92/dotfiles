{ }:
{
  deployment.keys."initrd-ssh-keys".text = ../secrets/initrd-ssh-key;

  initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 2222;
      hostECDSAKey = "/run/keys/initrd-ssh-key";
    };
    postCommands = ''
      echo "cryptsetup-askpass" >> /root/.profile

      ip addr add ${network.ipv6} dev eth0
      ip route add default via fe80::1 dev eth0
    '';
  };
}
