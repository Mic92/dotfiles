{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../modules/users.nix
    ../modules/minimal-docs.nix
    ../modules/networkd.nix
    ./hardware-configuration.nix
  ];

  # Fan speed adjustment
  systemd.services.fans = {
    wantedBy = ["multi-user.target"];
    serviceConfig.ExecStart = pkgs.runCommandCC "fans" {nativeBuildInputs = [pkgs.rustc];} ''
      rustc ${./fancontrol.rs} -o $out
    '';
    serviceConfig.Restart = "always";
  };

  services.openssh.enable = true;

  users.mutableUsers = false;
  users.users.root.password = "fnord23";

  networking.useNetworkd = true;
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  environment.systemPackages = with pkgs; [
    vim
    ethtool
    iperf
    parted
    fio
    wget
  ];

  boot.supportedFilesystems = ["zfs"];
  networking.hostId = "ac174b52";
}
