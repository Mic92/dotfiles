{ config, pkgs, lib, modulesPath, ... }:
{
  imports = [
    "${modulesPath}/installer/scan/not-detected.nix"
    ./modules/borg
    ../modules/users.nix
    ../modules/mosh.nix
    ../modules/tracing.nix
    ../modules/packages.nix
    ../modules/zfs.nix
    ../modules/libvirt.nix
    ../modules/intel-graphics.nix
    ../modules/tor-ssh.nix
    ../modules/networkd.nix
    ../modules/builder.nix
    ../modules/i3.nix
    ./modules/ping-tracker
    ./modules/sops.nix
    #./modules/awesome.nix
    #./modules/xfce.nix
  ];

  networking.retiolum = {
    ipv4 = "10.243.29.170";
    ipv6 = "42:0:3c46:126:3b9d:8298:f1b5:86d";
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
      IPv6Token=::521a:c5ff:fefe:65d9

      [DHCP]
      UseHostname=false
      RouteMetric=512
    '';
    usb.extraConfig = ''

      [Match]
      Name=eth1

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
      EmitRouter=yes
      DNS=8.8.8.8
    '';
  };
  hardware.pulseaudio.enable = true;

  sops.secrets.initrd-ssh-key = {};
  boot = {
    initrd.network = {
      enable = true;
      udhcpc.extraArgs = ["--background"];
      ssh = {
        enable = true;
        port = 2222;
        hostKeys = [
          config.sops.secrets.initrd-ssh-key.path
        ];
      };
      postCommands = ''
        echo "zfs load-key -a && killall zfs && ip addr flush dev eth0" >> /root/.profile
      '';
    };
  };

  services = {
    xrdp = {
      enable = true;
      defaultWindowManager = "i3";
    };

    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.gutenprint ];
    };
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    22 # ssh
    3389 # xrdp
    5900 # vnc in vagrant
    655 # tinc
  ];
  networking.firewall.allowedUDPPorts = [
    655
    53
    # dhcp
    67 68
  ];

  networking.nameservers = [ "1.1.1.1" ];

  networking.firewall.trustedInterfaces = [ "virbr+" "docker0" ];

  networking.firewall.allowedUDPPortRanges = [
    # mosh
    { from = 60000; to = 61000; }
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "e1000e" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelPackages = pkgs.linuxPackages;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "eddie"; # Define your hostname.

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
      storageDriver = "zfs";
      extraOptions = "--storage-opt=zfs.fsname=zroot/docker";
    };
    #virtualbox.host.enable = true;
    #virtualbox.host.headless = false;
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
    options = ["nofail"];
  };

  fileSystems."/home" = {
    device = "zroot/root/home";
    fsType = "zfs";
    options = ["nofail"];
  };

  fileSystems."/tmp" = {
    device = "zroot/root/tmp";
    fsType = "zfs";
    options = ["nofail"];
  };

  powerManagement.cpuFreqGovernor = "powersave";

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  users.extraUsers.sshjump = {
    shell = "/run/current-system/sw/bin/bash";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDoi3DBYWIyvXOjbhxPesfTYHe7++qSE9Ynq6En/jpUUAo6r2RF8mD2MHImRQ7D0NP15cQ1t9HcZQqeWPynAupftW7ECXKpo5eFm9mDWc/bhWHU2OSsgMSOzHaHNw9p8cqw2vwEShJDRfXcnXRk+Eue5Yj3FuJvImbkSxRQeoLEZC+apDvxxz6YdJrkaCDXsSqcjGq84Yp6EYV23/Jr4vHeHjRwjIUdVd1aGs/0j95f/zBrFrqa2WV5tCTl1bIyiJoK4dfy5RLzJCZva9k+qmA24AGQgfXdZl870LARweg+9vdxSzaMJgaKSZ4hYfjXNAj42kKxM5jjeptQvhhzZ+7cKbDnJFl+kEH1psQEaQ2xh4tLcQ7KSWW77voizBUujYA//Lbekaia1Js7jEl76Mp8FU/+5TWFkDoXypHu9z9JxEX2dz1ylEAQXfpyzZSzKLMz/VX++QmYt5CpbpsvfXdnLR+VCMBh2xb3mUEZ6p3fXJUmtvmHgnwvb8Yx3vgjvYs= joerg@turingmachine"
    ];
  };

  services.openssh.extraConfig = ''
    Match User sshjump
      AllowAgentForwarding no
      AllowTcpForwarding yes
      X11Forwarding no
      PermitTunnel no
      GatewayPorts no
      ForceCommand echo 'This account can only be used for ProxyJump (ssh -J)'
  '';

  environment.systemPackages = with pkgs; [ usbutils wireguard ];

  time.timeZone = "Europe/London";

  system.stateVersion = "18.03";
}
