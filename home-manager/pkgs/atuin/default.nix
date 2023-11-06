{ atuin }:
atuin.overrideAttrs (old: {
  # as cursed as doing mitigations=off in the kernel command line
  patches = [ ./0001-make-atuin-on-zfs-fast-again.patch ];
})
