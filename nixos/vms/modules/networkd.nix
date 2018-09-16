{ ... }: {
  # often hangs
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking.dhcpcd.enable = false;

  #services.resolved.enable = false;
}
