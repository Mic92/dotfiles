{ config, pkgs, ... }: {
  # Better than tlp? https://github.com/NixOS/nixos-hardware/pull/669#issuecomment-1632099013
  #services.power-profiles-daemon.enable = true;
  services.tlp.enable = false;
  hardware.system76.power-daemon.enable = true;

  services.udev.extraRules = ''
    ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${config.boot.kernelPackages.system76-power}/bin/system76-power profile battery"
    ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${config.boot.kernelPackages.system76-power}/bin/system76-power profile balanced"
  '';
}
