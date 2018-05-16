with import <nixpkgs> {};

{
  allowUnfree = true;
  pulseaudio = true;
  chromium.enablePepperPDF = true;

  packageOverrides = pkgs: with pkgs; {
    stable = import <stable> {};

    myVimBundle = { python ? python3 }: let
      plugins = (vimPlugins.override (old: { inherit python; }));
    in with plugins; {
       # loaded on launch
       start = [
         fugitive
         rhubarb
         vim-grammarous
         vim-docbk
         vim-docbk-snippets
         UltiSnips
         vim-autoformat
         fzfWrapper
         vim-devicons
         nvim-completion-manager
         LanguageClient-neovim
         nvim-cm-racer
         airline
         nerdtree
         nerdtree-git-plugin
         colors-solarized
         ack-vim
         vim-go
         vim-polyglot
         ale
         # delimitMat
         editorconfig-vim
         ctrlp
         rust-vim
         vim-yapf
         vim-signify
       ];
    };

    myvim = neovim.override {
      vimAlias = true;
      withPython = true;
      configure = {
        customRC = ''
          if filereadable($HOME . "/.vimrc")
            source ~/.vimrc
          endif
          let $RUST_SRC_PATH = '${rustPlatform.rustcSrc}'
          let g:grammarous#show_first_error = 1
        '';
        packages.nixbundle = myVimBundle {};
      };
    };

    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };
  };
}
