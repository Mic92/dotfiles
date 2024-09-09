{
  atuin,
  stdenv,
}:
if stdenv.isLinux then
  atuin.overrideAttrs (old: {
    # as cursed as doing mitigations=off in the kernel command line
    patches = (old.patches or [ ]) ++ [ ./0001-make-atuin-on-zfs-fast-again.patch ];
  })
else
  atuin
