{ pkgs, ... }:
let
  sshKey = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine" ];
  # Locate your wifi device with `lspci -v | less`
  # In this example the output is:
  # aa:00.0 Network controller: Intel Corporation Wi-Fi 6 AX210/AX211/AX411 160MHz (rev 1a)
  #         Subsystem: Intel Corporation Wi-Fi 6 AX210 160MHz
  #         Flags: bus master, fast devsel, latency 0, IRQ 17, IOMMU group 24
  #         Memory at 7a200000 (64-bit, non-prefetchable) [size=16K]
  #         Capabilities: <access denied>
  #         Kernel driver in use: iwlwifi
  #         Kernel modules: iwlwifi
  pciId = "aa:00.0";
  fullPcieId = "0000:${pciId}";

  vmBaseConfig = ({ config, ... }: {
    users.users.root.openssh.authorizedKeys.keys = sshKey;
    #networking.useNetworkd = true;

    # Only allow ssh on internal tap devices
    networking.firewall.interfaces.management.allowedTCPPorts = [ 22 ];
    services.openssh.openFirewall = false;

    # Needed for many wifi/ethernet cards
    hardware.enableRedistributableFirmware = true;

    services.openssh.enable = true;
    system.stateVersion = "23.11";

    # Faster evaluation speed because the manual doesn't need to be generated
    documentation.enable = false;

    # Might be more secure?
    #microvm.hypervisor = "cloud-hypervisor";
    microvm.storeDiskType = "squashfs"; # faster rebuilds because of parallelization, but slower boot
    microvm.interfaces = [{
      type = "tap";
      id = "mgt-${config.networking.hostName}";
      # since we do not have a common bridge we can use a fixed mac address for all vms
      mac = "02:00:00:01:01:01";
    }];

    systemd.network = {
      enable = true;
      links."05-management" = {
        matchConfig.MACAddress = "02:00:00:01:01:01";
        linkConfig.Name = "management";
      };
      networks."05-management" = {
        matchConfig.Name = "management";
        networkConfig.Address = "fe80::2/64";
      };
    };
  });

  internetAccess = ({ config, ... }: {
    microvm.interfaces = [{
      type = "tap";
      id = "vm-${config.networking.hostName}";
      # networkd will randomly assign a mac address
      mac = "02:00:00:01:01:02";
    }];
    systemd.network = {
      enable = true;
      links."05-internet" = {
        matchConfig.MACAddress = "02:00:00:01:01:02";
        linkConfig.MACAddressPolicy = "random";
        linkConfig.Name = "internet";
      };
      networks."05-internet" = {
        matchConfig.Name = "internet";
        networkConfig.DHCP = "yes";
      };
    };
  });
in
{
  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
  };
  # needed for flakes
  environment.systemPackages = [
    pkgs.git
    (pkgs.writeScriptBin "ssh-vm" ''
      #!/usr/bin/env bash
      if [[ "$#" -ne 1 ]]; then
        echo "Usage: $0 <vm-name>"
        exit 1
      fi
      vm=$1
      shift
      # we can disable host key checking because we use ipv6 link local addresses and no other vm can spoof them on this interface
      ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$@" root@fe80::2%mgt-$vm
    '')
  ];

  # network-manager should not mess with our tap devices
  networking.networkmanager.unmanaged = [ "interface-name:vt-*" ];

  # Nested MicroVMs
  microvm.vms.internet = {
    inherit pkgs;
    config = { ... }: {
      imports = [ vmBaseConfig ];
      # We use network manager to manage wifi.
      #networking.networkmanager.enable = true;
      #networking.networkmanager.unmanaged = [
      #  "interface-name:management"
      #  "interface-name:lan"
      #];
      networking.wireless.iwd.enable = true;

      # The qbios image seems to break pcie passthrough of my wifi card
      microvm.qemu.bios.enable = false;
      microvm.devices = [{
        bus = "pci";
        path = fullPcieId;
      }];

      microvm.interfaces = [{
        type = "tap";
        id = "tap-internet";
        # networkd will randomly assign a mac address
        mac = "02:00:00:01:01:02";
      }];

      # Allow DHCP server
      networking.firewall.allowedUDPPorts = [ 67 ];

      systemd.network = {
        enable = true;
        links."05-internet-bridge" = {
          matchConfig.MACAddress = "02:00:00:01:01:02";
          linkConfig.MACAddressPolicy = "random";
          linkConfig.Name = "lan";
        };
        networks."05-internet-bridge" = {
          matchConfig.Name = "lan";

          networkConfig = {
            DHCPServer = true;
            IPv6SendRA = true;
          };
          addresses = [
            # these should not collide with any other subnets
            { addressConfig.Address = "192.168.212.1/24"; }
            { addressConfig.Address = "fd4b:9650:cf30:0::/64"; }
          ];
          ipv6Prefixes = [{ ipv6PrefixConfig.Prefix = "fd4b:9650:cf30:0::/64"; }];
        };

        networks."wifi" = {
          matchConfig.Type = "wlan";

          networkConfig = {
            DHCPServer = true;
            IPv6SendRA = true;
          };
          addresses = [
            # these should not collide with any other subnets
            { addressConfig.Address = "192.168.212.1/24"; }
            { addressConfig.Address = "fd4b:9650:cf30:0::/64"; }
          ];
          ipv6Prefixes = [{ ipv6PrefixConfig.Prefix = "fd4b:9650:cf30:0::/64"; }];
        };
      };
      # just for network debugging
      environment.systemPackages = [
        pkgs.tcpdump
      ];

      networking.nat = {
        enable = true;
        enableIPv6 = true;
        internalInterfaces = [ "lan" ];
      };
      #microvm.shares = [{
      #  tag = "ro-store";
      #  source = "/nix/store";
      #  mountPoint = "/nix/.ro-store";
      #}];
    };
  };

  microvm.vms.wallet = {
    inherit pkgs;
    config = { ... }: {
      imports = [
        vmBaseConfig
        internetAccess
      ];
      microvm.hypervisor = "qemu";
      microvm.mem = 2000; # 2GB

      # breaks display
      microvm.qemu.bios.enable = false;
      microvm.qemu.extraArgs = [
        "-vnc"
        ":0"
        "-vga"
        "std"
        # needed for mounse/keyboard input via vnc
        "-device"
        "virtio-keyboard"
        "-usb"
        "-device"
        "usb-tablet,bus=usb-bus.0"
      ];

      users.users.user = {
        group = "user";
        isNormalUser = true;
        extraGroups = [ "video" ];
      };
      users.groups.user = { };

      hardware.enableRedistributableFirmware = true;
      hardware.opengl.enable = true;

      environment.systemPackages = [ pkgs.electrum ];

      services.xserver = {
        enable = true;
        desktopManager.xfce.enable = true;
        layout = "us";
        xkbVariant = "altgr-intl";
        xkbOptions = "caps:ctrl_modifier,compose:menu";
        displayManager.autoLogin.user = "user";
        displayManager.sessionCommands = ''
          ${pkgs.electrum}/bin/electrum &
        '';
      };

      microvm.shares = [{
        tag = "share";
        source = "/tmp/share";
        mountPoint = "/tmp/share";
      }];
    };
  };

  systemd.services.vfio-bind = {
    wantedBy = [ "multi-user.target" ];
    before = [ "microvm-wifi.service" ];
    path = [ pkgs.kmod ];
    script = ''
      set -euxo pipefail
      if [[ -d /sys/bus/pci/devices/${fullPcieId}/driver ]]; then
        driver=$(realpath /sys/bus/pci/devices/${fullPcieId}/driver)
        # unbind if not vfio:
        if [[ "$driver" == *vfio* ]]; then
          exit 0 # already bound
        else
          echo "${fullPcieId}" > /sys/bus/pci/devices/${fullPcieId}/driver/unbind
        fi
      fi
      modprobe vfio-pci
      # Read the file and extract vendor and device id
      while IFS= read -r line; do
        if [[ "$line" == PCI_ID=* ]]; then
          # Extract vendor and device ID
          ID=''${line#*=}
          VENDOR_ID=''${ID%%:*}
          DEVICE_ID=''${ID##*:}
          echo "Vendor ID: $VENDOR_ID"
          echo "Device ID: $DEVICE_ID"
        fi
      done < "/sys/bus/pci/devices/${fullPcieId}/uevent"
      echo "$VENDOR_ID $DEVICE_ID" > /sys/bus/pci/drivers/vfio-pci/new_id
    '';
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
  };

  networking.useNetworkd = true;
  networking.useDHCP = false;
  # Hetzner servers commonly only have one interface, so its either to just match by that.
  networking.usePredictableInterfaceNames = false;

  systemd.network = {
    enable = true;
    netdevs.internet.netdevConfig = {
      Kind = "bridge";
      Name = "internet";
    };

    networks."5-internet" = {
      matchConfig.Name = "internet";
      networkConfig.DHCP = "yes";
    };

    networks.microvm-internet = {
      matchConfig.Name = "vm-*";
      networkConfig.Bridge = "internet";
    };

    networks.tap-internet = {
      matchConfig.Name = "tap-internet";
      networkConfig.Bridge = "internet";
    };
  };
}
