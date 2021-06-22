{
  networking.hostName = "eva";

  time.timeZone = "UTC";
  i18n.defaultLocale = "en_DK.UTF-8";

  programs.vim.defaultEditor = true;

  imports = [
    ./modules/go-neb.nix
    ./modules/hardware-configuration.nix
    ./modules/prometheus
    ./modules/loki.nix
    ./modules/nginx.nix
    ./modules/sshd.nix
    ./modules/sops.nix
    ./modules/telegraf

    ../modules/mosh.nix
    ../modules/iperf.nix
    ../modules/openldap/replica.nix
    ../modules/promtail.nix
    ../modules/tracing.nix
    ../modules/users.nix
  ];

  systemd.network.networks."ethernet".extraConfig = ''
    [Match]
    Type = ether

    [Network]
    DHCP = yes
    LLMNR = true
    LinkLocalAddressing = yes
    LLDP = true
    IPv6AcceptRA = true
    IPForward = yes

    Address = 2a01:4f8:1c1c:9a9::1/128
    Gateway = fe80::1
    IPv6AcceptRA = no
    IPForward = yes
  '';

  systemd.network.enable = true;

  # breaks loki
  networking.usePredictableInterfaceNames = false;

  documentation.enable = false;

  networking.retiolum = {
    ipv4 = "10.243.29.185";
    ipv6 = "42:0:3c46:8a42:2b1:5ef8:7562:676a";
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "20.03";
}
