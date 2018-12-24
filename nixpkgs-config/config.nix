{
  allowUnfree = true;
  pulseaudio = true;
  #allowUnsupportedSystem = true;
  chromium.enablePepperPDF = true;

  packageOverrides = pkgs: let
    myVimBundle = with pkgs.vimPlugins; {
       # loaded on launch
       start = [
         fugitive
         rhubarb
         vim-grammarous
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
         ctrlp
         rust-vim
         vim-signify
         vim-nix
       ];
     };
   in {
    myVimBundle = myVimBundle;

    nur = pkgs.callPackage (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/master.tar.gz";
    })) {};

    myvim = pkgs.neovim.override {
      vimAlias = true;
      withPython = true;
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
