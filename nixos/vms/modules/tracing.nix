{ pkgs, config, ... }:
{
  programs.sysdig.enable = true;
  programs.bcc.enable = true;

  environment.systemPackages = [
    pkgs.strace
    config.boot.kernelPackages.perf
  ];
}
