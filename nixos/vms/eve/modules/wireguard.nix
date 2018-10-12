{ pkgs, config, ... }: {
  boot.extraModulePackages = with config.boot.kernelPackages; [
    wireguard
  ];
  environment.systemPackages = [ pkgs.wireguard-tools ];
}
