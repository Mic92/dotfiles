{ aerc }:

aerc.overrideAttrs (oldAttrs: {
  patches = (oldAttrs.patches or [ ]) ++ [
    ./patches/aerc-compose-pipe.patch
  ];
})
