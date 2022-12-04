{
  powerManagement.powertop.enable = true;
  # FIXME always coredumps on boot
  systemd.services.powertop.serviceConfig.Restart = "on-failure";
  systemd.services.powertop.serviceConfig.RestartSec = "2s";
}
