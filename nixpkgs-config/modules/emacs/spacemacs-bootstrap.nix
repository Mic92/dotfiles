with import <nixpkgs> { };

(callPackage ./spacemacs-with-packages.nix { }) {
  layers = ls: [
    ls.spacemacs-bootstrap
  ];
}
