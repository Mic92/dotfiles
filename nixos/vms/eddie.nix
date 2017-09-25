{
  eddi =
    { config, pkgs, lib, ... }:
    {
      deployment.targetHost = "129.215.90.4";

      imports = [
        <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
        ./modules/users.nix
        ./modules/retiolum.nix
      ];

      networking.retiolum = {
        ipv4 = "10.243.29.170";
        ipv6 = "42:4992:6a6d:700::1";
      };

      boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModulePackages = [ ];

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      networking.hostName = "eddie"; # Define your hostname.

      # for zfs
      networking.hostId = "81d75a04";
      boot.zfs.enableUnstable = true;

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

      environment.systemPackages = with pkgs; [
        config.boot.kernelPackages.perf
        config.boot.kernelPackages.bcc
        usbutils
        (sysdig.overrideDerivation (old: { dontStrip = true; }))
        wireguard
        socat
        whois

        # must have
        psmisc
        p7zip
        sipcalc
        iperf
        pkgconfig
        openssl
        binutils
        file
        wget
        htop
        ag
        lsof
        tcpdump
        tmux
        rsync
        git
        tig
        ruby.devEnv
        python
        python3
        go
        gcc
        strace
        ltrace
        nethogs
        iotop
        gnumake
        manpages
        dnsutils
        netcat
        mtr
        ntfs3g

        arc-icon-theme
        arc-theme
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

      users.extraUsers.fsgqa = {
        isNormalUser = true;
        home = "/home/joerg";
      };

      system.stateVersion = "18.03";
    };
}
