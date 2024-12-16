{
  pkgs,
  self,
  inputs,
  ...
}:
{
  imports = [
    self.nixosModules.default
    inputs.srvos.nixosModules.server
    inputs.clan-core.clanModules.state-version

    ../matchbox/modules/samba.nix
    ../matchbox/modules/rsyncd.nix
    ../matchbox/modules/rsnapshot.nix
    ../matchbox/modules/photoprism.nix

    ../../nixosModules/users.nix
    ../../nixosModules/mosh.nix
    ../../nixosModules/sshd/tor.nix
    ../../nixosModules/promtail.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;

  clan.core.networking.targetHost = "root@192.168.188.106";
  #clan.core.networking.buildHost = "root@eve.i";
  clan.core.deployment.requireExplicitUpdate = true;

  time.timeZone = "UTC";

  services.getty.autologinUser = "root";

  environment.systemPackages = with pkgs; [
    tmux
    htop
    iotop
    tcpdump
    strace
    ethtool
    usbutils
    bandwhich
    vim
  ];

  systemd.network.networks.ethernet = {
    matchConfig.Type = "ether";
    networkConfig = {
      DHCP = true;
      LLMNR = true;
      LinkLocalAddressing = true;
      LLDP = true;
      IPv6AcceptRA = true;
    };
    dhcpConfig = {
      UseHostname = false;
      RouteMetric = 512;
    };
    extraConfig = ''
      [Network]
      IPv6Token = "::fd87:20d6:a932:6605";
    '';
  };

  services.resolved.enable = true;
}
