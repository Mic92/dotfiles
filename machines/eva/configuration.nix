{
  lib,
  pkgs,
  self,
  ...
}:
{
  networking.hostName = "eva";

  time.timeZone = "UTC";

  imports = [
    self.nixosModules.default
    self.inputs.srvos.nixosModules.server
    self.inputs.srvos.nixosModules.mixins-nginx
    self.inputs.srvos.nixosModules.mixins-systemd-boot
    self.inputs.srvos.nixosModules.roles-prometheus
    self.inputs.srvos.nixosModules.hardware-hetzner-cloud
    self.inputs.disko.nixosModules.disko

    ./modules/disko.nix
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/prometheus
    ./modules/telegraf
    ./modules/zerotier.nix

    ../../nixosModules/nncp.nix
    ../../nixosModules/iperf.nix
    ../../nixosModules/openldap/replica.nix
    ../../nixosModules/promtail.nix
    ../../nixosModules/users.nix
    ../../nixosModules/unbound.nix
    ../../nixosModules/hyprspace-public.nix
  ];
  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.aarch64-linux;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.systemd.enable = true;
  clan.core.networking.targetHost = lib.mkForce "root@eva.i";
  clan.core.networking.buildHost = "root@eve.i";

  # Disable envfs to fix systemd refusing to run with unpopulated /usr/
  services.envfs.enable = lib.mkForce false;

  # Use stable kernel
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages;

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
