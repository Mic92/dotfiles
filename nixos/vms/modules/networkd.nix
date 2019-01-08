{ pkgs, ... }: {
  systemd.network.enable = true;

  # often hangs
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking.dhcpcd.enable = false;
}
