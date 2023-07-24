{ lib, pkgs, ... }: {
  networking.hostName = "eva";

  time.timeZone = "UTC";

  programs.vim.defaultEditor = true;

  imports = [
    ./modules/go-neb.nix
    ./modules/hardware-configuration.nix
    ./modules/prometheus
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/sops.nix

    ../modules/sshd
    ../modules/mosh.nix
    ../modules/iperf.nix
    ../modules/openldap/replica.nix
    ../modules/promtail.nix
    ../modules/users.nix
    ../modules/unbound.nix
    ../modules/zerotier.nix
  ];

  boot.initrd.systemd.enable = false;

  systemd.network = {
    enable = true;
    networks."eth0".extraConfig = ''
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
  system.stateVersion = "20.03";
}
