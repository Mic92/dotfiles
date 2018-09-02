{
  rock =
    { config, pkgs, lib, ... }:
    {
      deployment.targetHost = "borg.r";

      nixpkgs.localSystem.system = "aarch64-linux";

      imports = let
        rock64 = builtins.fetchTarball "https://github.com/thefloweringash/rock64-nix/archive/master.tar.gz";
      in [
        (rock64 + "/modules/rock64-configuration.nix")
        (rock64 + "/modules/packages.nix")
        ./modules/users.nix
        ./modules/retiolum.nix
        ./modules/mosh.nix
        ./modules/overlay.nix
        ./modules/tor-ssh.nix
        ./modules/xfce.nix
        ./modules/networkd.nix
        ./modules/nix-daemon.nix
        ./modules/dashboard.nix
      ];

      boot.kernelPackages = pkgs.rock64.linuxPackages_ayufan_4_4;

      networking.hostName = "rock";

	    networking.retiolum = {
	    	ipv4 = "10.243.29.171";
	    	ipv6 = "42:4992:6a6d:700::2";
      };

      systemd.network.networks = {
        ethernet.extraConfig = ''
          [Match]
          Name=eth0

          [Network]
          DHCP=both
          LLMNR=true
          IPv4LL=true
          LLDP=true
          IPv6AcceptRA=true
          IPv6Token=::fd87:20d6:a932:6605

          [DHCP]
          UseHostname=false
          RouteMetric=512
        '';
      };

      fileSystems."/" = {
        device = "/dev/mmcblk1p2";
        fsType = "ext4";
      };

      services.xrdp = {
        enable = true;
        defaultWindowManager = "xfce4-session";
      };

      environment.systemPackages = with pkgs; [
        tmux
        htop
        iotop
        tcpdump
        strace
        ethtool
      ];

      system.stateVersion = "18.03";
    };
}
