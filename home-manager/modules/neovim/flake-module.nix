{ inputs, ... }: {
  perSystem = { self', pkgs, ... }: {
    packages = {
      astro-nvim-config = pkgs.callPackage ./astro-nvim-config.nix { inherit inputs; };
      nvim-open = pkgs.python3Packages.callPackage ./nvim-open.nix { };
      nvim = pkgs.callPackage ./nvim-standalone.nix {
        inherit (self'.packages) astro-nvim-config;
        nvim-appname = "mic92-vim";
      };
    };
  };
}
