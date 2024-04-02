{ lib, ... }:
{
  perSystem =
    {
      self',
      inputs',
      pkgs,
      ...
    }:
    {
      packages = {
        nvim-open = pkgs.python3Packages.callPackage ./nvim-open.nix { };
        nvim = pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped (
          pkgs.neovimUtils.makeNeovimConfig {
            wrapRc = false;
            extraLuaPackages = ps: [ (ps.callPackage ./lua-tiktoken.nix { }) ];
          }
        );
        treesitter-grammars = pkgs.runCommand "treesitter-grammars" { } (
          lib.concatMapStringsSep "\n" (grammar: ''
            mkdir -p $out
            ln -s $(readlink -f ${grammar}/parser/*.so) $out/${lib.last (builtins.split "-" grammar.name)}.so
          '') pkgs.vimPlugins.nvim-treesitter.withAllGrammars.dependencies
        );
      };
    };
}
