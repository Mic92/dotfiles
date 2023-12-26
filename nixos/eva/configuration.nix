{ lib, pkgs, ... }: {
  networking.hostName = "eva";

  time.timeZone = "UTC";

  programs.vim.defaultEditor = true;

  # we use age instead of ssh keys
  sops.age.sshKeyPaths = lib.mkForce [ ];
  sops.gnupg.sshKeyPaths = lib.mkForce [ ];

  imports = [
    ./modules/disko.nix
    ./modules/hardware-configuration.nix
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/prometheus
    ./modules/telegraf

    ../modules/sshd
    ../modules/mosh.nix
    ../modules/nncp.nix
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
