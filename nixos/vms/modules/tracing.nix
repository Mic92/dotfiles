{ pkgs, config, ... }:
{
  programs.bcc.enable = true;
  programs.sysdig.enable = true;
  systemd.coredump.enable = true;

  environment.systemPackages = [
    pkgs.strace
    config.boot.kernelPackages.perf
  ];
}
