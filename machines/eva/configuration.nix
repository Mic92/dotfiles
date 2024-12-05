{
  lib,
  pkgs,
  self,
  inputs,
  ...
}:
{
  networking.hostName = "eva";

  time.timeZone = "UTC";

  imports = [
    self.nixosModules.default
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.mixins-nginx
    inputs.srvos.nixosModules.mixins-systemd-boot
    inputs.srvos.nixosModules.roles-prometheus
    inputs.disko.nixosModules.disko

    ./modules/disko.nix
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/prometheus
    ./modules/telegraf
    ./modules/zerotier.nix

    ../../nixosModules/mosh.nix
    ../../nixosModules/nncp.nix
    ../../nixosModules/iperf.nix
    ../../nixosModules/openldap/replica.nix
    ../../nixosModules/promtail.nix
    ../../nixosModules/users.nix
    ../../nixosModules/unbound.nix
    ../../nixosModules/hyprspace.nix
  ];
  nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.systemd.enable = false;
  clan.core.networking.targetHost = "root@eva.i";
  clan.core.networking.buildHost = "root@eve.i";

  systemd.network = {
    enable = true;
    networks."40-eth0".extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      Address = 89.58.27.144/22
      Gateway = 89.58.24.1
      Address = 2a03:4000:62:fdb::/64
      Gateway = fe80::1
      IPv6AcceptRA = no
      IPForward = yes

      [DHCP]
      UseDNS = no
    '';
  };

  services.resolved.enable = false;

  # breaks loki
  networking.usePredictableInterfaceNames = false;

  # lxc-container.nix sets this
  documentation.enable = lib.mkForce false;

  environment.systemPackages = [
    pkgs.htop
    pkgs.tmux
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "23.11";
}
