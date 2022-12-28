{ config
, lib
, ...
}: {
  programs.mosh.enable = true;

  networking.firewall.allowedUDPPortRanges = lib.optionals config.services.openssh.enable [
    # Mosh
    {
      from = 60000;
      to = 60010;
    }
  ];
}
