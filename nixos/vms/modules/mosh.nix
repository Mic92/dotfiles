{ pkgs, config, lib, ... }: {
  programs.mosh.enable = true;

  networking.firewall.allowedUDPPortRanges = lib.optionals (config.openssh.enable) [
    # Mosh
    { from = 60000; to = 60010; }
  ];
}
