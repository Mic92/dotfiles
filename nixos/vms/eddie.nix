{

  #libvirtExampe = { ... }: {
  #    deployment.targetEnv = "libvirtd";
  #    deployment.libvirtd.headless = true;
  #    users.users.root.initialPassword = "root";
  #}

  eddie =
    { config, pkgs, lib, ... }:
    {
      #deployment.targetHost = "129.215.90.4";
      deployment.targetHost = lib.mkForce "eddie.r";

      deployment.keys."initrd-ssh-key".keyFile = ../secrets/eddie/initrd-ssh-key;

      imports = [
        <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
        ./modules/users.nix
        ./modules/retiolum.nix
        ./modules/mosh.nix
        ./modules/monit.nix
        ./modules/overlay.nix
        ./modules/tracing.nix
        ./modules/packages.nix
        ./modules/zfs.nix
      ];

      programs.ssh.startAgent = true;

      nix = {
        trustedUsers = ["joerg"];
        useSandbox = true;
        extraOptions = ''
          build-max-jobs = 10
        '';
      };

      networking.retiolum = {
        ipv4 = "10.243.29.170";
        ipv6 = "42:4992:6a6d:700::1";
      };

      networking.dhcpcd.enable = false;

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
          IPv6Token=::521a:c5ff:fefe:65d9

          [DHCP]
          UseHostname=false
          RouteMetric=512
        '';
        usb.extraConfig = ''
          [Match]
          Name=enp0s20f0u9

          [Network]
          Address=192.168.44.253/24
          IPMasquerade=true
          LLMNR=true
          IPv4LL=true
          LLDP=true
          DHCPServer=yes

          [DHCPServer]
          PoolOffset=100
          PoolSize=20
          EmitDNS=yes
          DNS=8.8.8.8
        '';
      };

      boot = {
        initrd.network = {
          enable = true;
          udhcpc.extraArgs = ["--background"];
          ssh = {
            enable = true;
            port = 2222;
            hostECDSAKey = "/run/keys/initrd-ssh-key";
          };
          postCommands = ''
            echo "zfs load-key -a; killall zfs; ip addr flush dev eth0" >> /root/.profile
          '';
        };
      };

      services = {
        nfs.server = {
          enable = true;
          exports = "/mnt/original 127.0.0.1(rw,no_root_squash)";
        };
        unbound = {
          enable = true;
          forwardAddresses = ["9.9.9.9"];
          allowedAccess = ["0.0.0.0/0" "::/0"];
          interfaces = ["10.243.29.170" "42:4992:6a6d:700::1"];
        };
        xserver = {
          enable = true;
          desktopManager.xfce.enable = true;
          layout = "us";
          xkbVariant = "altgr-intl";
          xkbOptions = "caps:ctrl_modifier,compose:menu";
        };
        xrdp = {
          enable = true;
          defaultWindowManager = "xfce4-session";
        };

        printing = {
          enable = true;
          browsing = true;
          drivers = [ pkgs.gutenprint ]; # pkgs.hplip
        };
      };

      networking.firewall.enable = true;
      networking.firewall.allowedTCPPorts = [
        3389 # xrdp
        655 # tinc
      ];
      networking.firewall.allowedUDPPorts = [
        655
        53
        # dhcp
        67 68
      ];
      networking.firewall.allowedUDPPortRanges = [ 
        # mosh
        { from = 60000; to = 61000; }
      ];

      boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "e1000e" ];
      boot.kernelModules = [ "kvm-intel" "wireguard" ];
      boot.kernelPackages = pkgs.linuxPackages_latest;
      boot.extraModulePackages = with config.boot.kernelPackages; [ wireguard ];

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      networking.hostName = "eddie"; # Define your hostname.

      virtualisation = {
        virtualbox.host.enable = true;
        docker = {
          enable = true;
          enableOnBoot = false;
          storageDriver = "zfs";
          extraOptions = "--storage-opt=zfs.fsname=zroot/docker";
        };
      };

      # for zfs
      networking.hostId = "81d75a04";
      boot.zfs.enableUnstable = true;

      services.zfs = {
        autoSnapshot.enable = true;
        autoScrub.enable = true;
      };

      fileSystems."/" = {
        device = "zroot/root/nixos";
        fsType = "zfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/5C7E-D359";
        fsType = "vfat";
      };

      fileSystems."/home" = {
        device = "zroot/root/home";
        fsType = "zfs";
      };

      fileSystems."/tmp" = {
        device = "zroot/root/tmp";
        fsType = "zfs";
      };

      swapDevices = [ ];

      nix.maxJobs = lib.mkDefault 4;

      powerManagement.cpuFreqGovernor = "powersave";

      services.avahi = {
        enable = true;
        nssmdns = true;
      };

      environment.systemPackages = with pkgs; [
        usbutils
        wireguard
      ];

      services.tor = {
        enable = true;
        hiddenServices."eddie".map = [
          { port = 22; }
          { port = 2015; }
        ];
        extraConfig = ''
          DNSPort 9053
          AutomapHostsOnResolve 1
          AutomapHostsSuffixes .exit,.onion
          EnforceDistinctSubnets 1
          ExitNodes {de}
          EntryNodes {de}
          NewCircuitPeriod 120
        '';
      };

      time.timeZone = "Europe/London";

      services.openssh.enable = true;

      system.stateVersion = "18.03";
    };
}
