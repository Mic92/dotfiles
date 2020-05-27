{ emacsWithPackages }:

{ layers, themes ? p: [ ] }:

emacsWithPackages (ps: layers (import ./all-spacemacs-packages.nix ps) ++ themes ps)
