{ ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      nixUnstable = super.nixUnstable.overrideAttrs
        (old: {
          patches = (old.patches or [ ]) ++ [
            # somehow import-from-derivation is broken
            (super.fetchpatch {
              url = "https://github.com/Mic92/nix/commit/9e535ae73f58b99c603887ed61e4df40964656a0.patch";
              sha256 = "sha256-/fKukZ6cwVceQD8PuYF4+NRlXNqBmEl+BmPQ3hblzUU=";
            })
          ];
          doInstallCheck = false;
        });
    })
  ];
}
