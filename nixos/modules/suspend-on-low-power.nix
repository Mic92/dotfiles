{ config, ... }:
let
  powerInPercent = 10;
in
{
  services.udev.extraRules = ''
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="${toString powerInPercent}", RUN+="${config.systemd.package}/bin/systemctl suspend"
  '';
}
