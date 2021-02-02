{ pkgs, nurFun ? null }: {
  allowUnfree = true;
  pulseaudio = true;
  #allowUnsupportedSystem = true;
  oraclejdk.accept_license = true;
  android_sdk.accept_license = true;

  packageOverrides = pkgs: let
    myVimBundle = with pkgs.vimPlugins; {
       # loaded on launch
       start = [
         fugitive
         rhubarb
         vim-husk
         UltiSnips
         vim-autoformat
         fzfWrapper
         vim-devicons
         ncm2
         ncm2-bufword
         ncm2-path
         ncm2-tmux
         ncm2-ultisnips
         vim-snippets
         nvim-yarp
         LanguageClient-neovim
         airline
         nerdtree
         nerdtree-git-plugin
         gruvbox
         ack-vim
         vim-go
         vim-polyglot
         ale
         # delimitMat
         editorconfig-vim
         fzf-vim
         rust-vim
         vim-signify
         vim-nix
       ];
     };
   in {
    myVimBundle = myVimBundle;

    nur = nurFun {
      nurpkgs = pkgs;
      inherit pkgs;
    };

    bitwarden-wrapper = pkgs.callPackage ./bitwarden.nix {};

    myvim = pkgs.neovim.override {
      vimAlias = true;
      withPython = false;
      configure = {
        customRC = ''
          if filereadable($HOME . "/.vimrc")
            source ~/.vimrc
          endif
          let $RUST_SRC_PATH = '${pkgs.rustPlatform.rustcSrc}'
          let g:grammarous#show_first_error = 1
        '';
        packages.nixbundle = myVimBundle;
      };
    };
  };
}
