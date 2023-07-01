{ lib, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 9993 ];
  networking.firewall.allowedUDPPorts = [ 9993 ];
  services.zerotierone = {
    enable = true;
    joinNetworks = [
      "b15644912ea8230e" # krebs testnet on https://my.zerotier.com/network/b15644912ea8230e
      #"65af404b79112bbe"
    ];
  };
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "zerotierone"
  ];

  # we need this to autoassign 42::/16 ips
  systemd.services.zerotierone.serviceConfig = {
    ExecStartPost = pkgs.writers.writeDash "configure-zerotier" ''
      until ${pkgs.zerotierone}/bin/zerotier-cli set b15644912ea8230e allowGlobal=1; do
        sleep 1
      done
    '';
  };

  systemd.tmpfiles.rules = [
    "L+ /var/lib/zerotier-one/local.conf - - - - ${pkgs.writeText "local.conf" (builtins.toJSON {
      physical = {
        "10.243.0.0/16".blacklist = true;
        "10.244.0.0/16".blacklist = true;
        "10.250.0.0/16".blacklist = true;
        "42::/16".blacklist = true;
      };
      # virtual = {
      #   feedbeef12 = {
      #     role = "UPSTREAM";
      #     try = [ "10.10.20.1/9993" ];
      #     blacklist = [ "192.168.0.0/24" ];
      #   }
      # };
    })}"
  ];

  networking.networkmanager.unmanaged = [ "interface-name:zt*" ];
}

