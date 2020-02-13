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
        ./modules/tor-ssh.nix
        ./modules/xfce.nix
        ./modules/networkd.nix
        ./modules/nix-daemon.nix
        ./modules/dashboard.nix
        ./modules/netdata
      ];

      users.extraUsers.chris = {
        isNormalUser = true;
        extraGroups = [ "wheel" "input" ];
        openssh.authorizedKeys.keys = [
          ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDyUL/nNzETcpynDe4ifESg7JiC/Fnovd0CFuQKDu/2Z8n0x6lKr1WqXRN6jP3QWcTsB41epG1SQNvrAYk5X2+07mGpFecNmSbgsJvxs2YWKZyjWdiJJcVzIqDPV72jYOXIrX3/RhXyzKkKHwjjMIniZwCd6qbDGAN6xmxFTVE8Uk+E2fpCAPSSM44wNdPtAtCSxSMUSEE9/StooZ0KEeRFgVQQ53/zChfD2n16IovuT6E7VJPH7r9AAJmvQyWo/yE5PGKT1QcYWfgRpn0y0F4wLfHeUGcaQ2E/fndBODqeRdnBd88XocIvw2Kv+brKJo/c7Ht0j+OzD/EtIJ5OojO9''
          ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDeNNC/pFEjOks5M3WKFx07hmXa3e5mKBH5C4l/QLwrDeNZ+Jo/sjszmoXeXc08jyB8gKVt/ylXfVbRhM9x/IRH7+JLXYfXYuOBuj7YIb8QMlxYqnc7bF2uXmpC+Kd5X7M8o2Bbx+IOJAfuuI/cahzxJw2mnaJjcyJaJiltgWrafHZRFW+eMso9eob18+yCdGjcLQYeJTFP2+7XC+U2UnHtteRNW5IVbgyWdvOJZa0BBN6oXoF1obcPtBWrORjIMC/b5MYLqK0oW/hpbuBRRCNRDOG0246AFLRsiGHmwun21YRwrAaVWZI5nYsHb/P/vHb9XLp3kX2R24mfXzeB06KpjqKR1p/vHENqA/bx6DZTbGwmZIpy2hn6qKOdp7UICIJU9C9b/y8c7gRQDiESkjq8UD/RvykR9HJY/nh0BRjs2QQ5tG7JkfEHaljMpdjDVRu+c7zRzJyO37tyoI32Oq4+2wugh96sLnxcaQ+u70PK0R7V3UBcJTEXs9uU1FeVqszGhdb5wORjFtA8ZJUK3f7D3ECWjw7IPbDsyJXkjDroA+jAabIRCwYVQppkiu1kSPU8x2R+ZVwfbf2xWMacCMpGwdYz1vuorfZpgZ2qG1MYvgDFm2pL5/UJ2BtTDMK9O9MtNnHXLhPpW3WtTIEJNm4aQ0I3J4Zgo+Kfn9MwkUQVYw==''
          ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLJhMVxEI6ImRJCTq8x5FBBAH4ZGPYATUNA9rAErI70iHLQOLzfgVoD27SA94ml459NbVj77xhFM8bg3BcjXXbU5wY3vbW3D5rPCBPQjVrfhMp1KTVRIkFrqsTk8XbRXyFojjVlajhTvmR4ozsfP331vOYSR/HF913hlEi4HQlMMxhdOadRWW16RQ6pdWH86USMfDejhhc+ek4Om7U7FhdzPx0mQStCsjH2a4aMXHjk+PLK/smpDXfe6WufnLyK6Qo9r4aKmIObWLMWOjiIJlpGLuAH2R2B9OordR3HkXyHEJPNNIhSn0D3gYM474i6ZhNkNpOLxH3vfl7AlimHx6s4sAGQZG+2cdlPPvb6efZUiwcjjsbj/MRkf6AGdnSye3BKT/FttNPS8B0mf5/zO7537sS1xGi4H2LgrkHgQNA6+AEMyfb9JSwV2QiasVjT69+0ZTBA2zdQ8uc7mhfrYNROdMFbJTMMWUxau5NlTmWXLgcYj8nH7lxHgNc1K6NRlyc8Dh7rAFB3gzz8vwgg79U6phlAsm7RJ4+/62S1/yJleFpH/jdS8R6XarKclQm40NeEz/tz7U0KH2KS5RoLSwU/5QNgk7A6I2JqmFA6ZvK4o7JF5Jygmlpfi8U1KHNFEhtq+k/cxNo7qkguc6/euPhxEvIsGw+jkkt5+OihuEH0w==''
          ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDV9qD8H13PtobX64DhWsmGL+O2eLEHXw2VMOEDM2F4JvnLABwonp6/Id4ExdaVQ1LsHqPEo/NdlI5t7OYywbOmaF4D6hYupVfzZeSFb1QYFytXp530HURHpH9cT8EO3XFPWsGIN/FSC27NH21HYKn7+QdiAXHBeXxOT9ICHJL4NsVTw/E5T1QM55tZO5GnOUpX8CGLZvy0NN4KglR12/3r1iJelRl2AsHI+aKE+BVda0gSHBwBHazWPD2yPpqMj9VTvzMNlGWYdTV65C3wcEKSf+todZK+iyNGc6eOSD10kIt4XhC0/6/JTXE3RQn3bavAZh8wJ8pwgtXHMAs5ZwlkGKTcSGo4oriN4WS4E94O/Xgw5AbeCDgYLP98uIVMKQOw2eBqq6zVOy8Znwo8ICpqAo3sqdZmGBruSZX2TS2UTg1/asEPF2ykacLGECPGbK2cFNsST3Xtzj5QX0XPkAY/OvbxkfjYEi9KSNZATDRwMs1JXzjP4c6edOZd8ScA3Yk2ln1dx5AjQuWDwpxBfNkcXNsTuF8jOzoUTqjthKq/zz+CYJM6IFQF20uus8zWjM6sRC1tkFf5jWBfdtsA9PT2otSh628bBUd+dBY7AazBMKufwpJZsycs7GJ1CVyJ47iutpOSytjj7ORyW33DEH9CO24J/eqnVb0VfbGKGaimjQ==''
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

      systemd.services.netdata = {
        path = with pkgs; [ python3 ];
      };

      services.netdata.httpcheck.checks.dashboard = {
        url = "http://dashboard.thalheim.io:3030";
        regex = "office-dashboard";
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

      time.timeZone = "Europe/London";

      system.stateVersion = "18.03";
    };
}
