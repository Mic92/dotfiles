let
  sshKeys = (import ./ssh-keys.nix);
in
  (import ./kexec-installer.nix) {
    extraConfig = {pkgs, ...}: {
      environment.systemPackages = [ pkgs.vim ];
      services.openssh = {
        enable = true;
        startWhenNeeded = true;
      };
      users.extraUsers.root.openssh.authorizedKeys.keys = with sshKeys; alfred ++ joerg;
      networking = {
        firewall.allowedTCPPorts = [ 22 ];
        usePredictableInterfaceNames = false;
        useDHCP = false;
      };
      systemd.network.enable = true;
      environment.etc."systemd/network/eth0.network".text = ''
        [Match]
        Name = eth0

        [Network]
        DHCP=both
        Address = 64.137.198.241/24
        Gateway = 64.137.198.1
      '';
    };
  }
