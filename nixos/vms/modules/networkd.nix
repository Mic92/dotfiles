{ pkgs, ... }: {
  systemd.network.enable = true;
  services.resolved.enable = false;

  # often hangs
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking.dhcpcd.enable = false;
}
