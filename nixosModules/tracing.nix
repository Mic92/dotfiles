{ pkgs, lib, ... }:
let
  # Upstream perf disables libbfd by default (BUILD_NONDISTRO) due to GPLv3
  # licensing concerns for binary distribution. Without it perf falls back to
  # fork/exec'ing `addr2line` for every inline/srcline lookup, which makes
  # `perf report` on DWARF-unwound profiles unusably slow. We build from
  # source anyway, so just turn it back on.
  perfWithBfd = pkgs.perf.overrideAttrs (old: {
    makeFlags = (old.makeFlags or [ ]) ++ [ "BUILD_NONDISTRO=1" ];
    buildInputs = (old.buildInputs or [ ]) ++ [
      pkgs.libbfd
      pkgs.libiberty
    ];
    # bfd.h refuses to be included without PACKAGE/PACKAGE_VERSION defined
    # (autoconf guard). perf's own build doesn't set it when libbfd detection
    # is forced via BUILD_NONDISTRO.
    env = (old.env or { }) // {
      NIX_CFLAGS_COMPILE = (old.env.NIX_CFLAGS_COMPILE or "") + " -DPACKAGE=\"perf\"";
    };
  });
in
{
  programs.bcc.enable = true;
  programs.sysdig.enable = true;

  environment.systemPackages = [
    pkgs.strace

    # we want to use trace from bcc
    (lib.lowPrio perfWithBfd)
  ];
}
