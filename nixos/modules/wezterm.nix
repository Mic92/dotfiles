{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.wezterm ];
  networking.firewall.allowedTCPPortRanges = [
    # wezterm tls server
    {
      from = 60000;
      to = 60010;
    }
  ];
}
