{ pkgs, ... }: {
  systemd.network.enable = true;
  networking.useNetworkd = true;

  # often hangs
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking.dhcpcd.enable = false;

  # this leads to timeouts for some devices (virtualbox or tinc adapter)
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = ["" "${pkgs.coreutils}/bin/true"];
}
