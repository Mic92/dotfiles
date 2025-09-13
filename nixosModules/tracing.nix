{ pkgs, ... }:
{
  programs.bcc.enable = true;
  programs.sysdig.enable = true;

  environment.systemPackages = [
    pkgs.strace

    # we want to use trace from bcc
    (pkgs.lowPrio pkgs.perf)
  ];
}
