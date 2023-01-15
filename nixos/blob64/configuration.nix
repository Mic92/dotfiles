{ pkgs, ... }: {
  imports = [
    ./modules/borgbackup-repos
    ../modules/users.nix
    ../modules/minimal-docs.nix
    ../modules/networkd.nix
    ../modules/ip-update.nix
    #../modules/tailscale.nix
    ./hardware-configuration.nix
  ];

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  system.stateVersion = "22.11";

  # Fan speed adjustment
  systemd.services.fans = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = pkgs.runCommandCC "fans" { nativeBuildInputs = [ pkgs.rustc ]; } ''
      rustc ${./fancontrol.rs} -o $out
    '';
    serviceConfig.Restart = "always";
  };

  services.openssh.enable = true;

  users.mutableUsers = false;
  users.users.root.password = "fnord23";

  networking.useNetworkd = true;

  networking.hostName = "blob64";

  environment.systemPackages = with pkgs; [
    vim
    ethtool
    iperf
    parted
    fio
    wget
    htop
    tcpdump
    nmap
    python3
    tmux
  ];

  systemd.network.networks."10-uplink" = {
    matchConfig.Type = "ether";
    networkConfig.DHCP = "yes";
  };

  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";
}
