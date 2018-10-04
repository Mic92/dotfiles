{
  rock =
    { config, pkgs, lib, ... }:
    {
      deployment.targetHost = "rock.r";

      nixpkgs.localSystem.system = "aarch64-linux";

      imports = let
        #rock64 = builtins.fetchTarball "https://github.com/thefloweringash/rock64-nix/archive/master.tar.gz";
        rock64 = ./rock64-nix;
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

      users.extraUsers.chris = {
        isNormalUser = true;
        extraGroups = [ "wheel" "input" ];
        openssh.authorizedKeys.keys = [
          ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7KdbeVK4cCUIe4dV0SLAddFXGEzHlD/Ct1pFE6r7gXJWWUBWTks7pjfT0R8a5bB6ECrPWE1YlYonOlYGOEUE6GbgVZ+zNwHrmDz/I6P01YfQeJH7dbywVm1Hk70aynnbHhhvXKRySML0DPo7zq/JxV3YRA7TQ1Ywj4ZyUZYwz7i2/PG85llSt8UYNsasSweQJf4Qh/aSsDT6PHI6bpS1vjGERgAl2BarjBfZb3vBKSwgqVNLqnzFE3hHmEn29NJ9K4As6eAxB0PDKhSHV1kLGADPLcEH+F36xBGNta+oVP3SaUkUw8PQBq3h3g1U0hsqL+PELS3RTpWfV+0J57RQxgSH8HNtZqdlQjAuDWKOA4pF5qofN5rAjJ4VC2SmqPe45M/A97PCXb0xo378UnMEPUG4wHvhnU5TgseGN1D6S5wmRAXYUEixJpu6vmRLqPaubxw7PCovKwXdNE+6z5/787hF3ICK/RwK0NJekcmz8S5cRP5WkTgGDmeMLwYDcNqpyo7QdcGVDjoeY0bB9CQ3+cWrN/Vr1UNr68lsQGLdrR9IUmcHQrXiipbzzdfJF34SRuezUc3HOOv5mrSOctvJLsZVWwl4rT39aEmB0+Dq36r5aAKx4zN0uGy4pIS/y7IDe9t6KQoiou7nS10zRAf+vlroBFepkPVJm3fs0oVC6YQ==''
        ];
      };

      boot.kernelPackages = pkgs.rock64.linuxPackages_ayufan_4_4;

      networking.hostName = "rock";

	    networking.retiolum = {
	    	ipv4 = "10.243.29.171";
	    	ipv6 = "42:4992:6a6d:700::2";
      };

      networking.firewall.allowedTCPPorts = [
        3389 # xrdp
        655 # tinc
      ];

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

      time.timeZone = "Europe/London";

      system.stateVersion = "18.03";
    };
}
