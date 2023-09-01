{ writeShellScriptBin
, astro-nvim-config
, buildEnv
, vimPlugins
, neovim
, stdenv
, fetchFromGitHub
, luajit
}:
let
  # https://github.com/kmarius/jsregexp
  jsregexp = stdenv.mkDerivation {
    name = "jsregexp";
    src = fetchFromGitHub {
      owner = "kmarius";
      repo = "jsregexp";
      rev = "1f4fa8ff9570501230d88133537776869d333f12";
      sha256 = "sha256-vE2N1VKaEBeJ8IHuP+n0MwIzmkpgh/Ak50nWJUVqfgM=";
    };
    buildInputs = [ luajit ];
    installPhase = ''
      runHook preInstall
      install -m755 -D jsregexp.so $out/lib/jsregexp.so
      runHook postInstall
    '';
  };
in
writeShellScriptBin "vim" ''
  set -efux
  unset VIMINIT
  export PATH=$PATH:${buildEnv {
    name = "lsp-servers";
    paths = astro-nvim-config.lspPackages;
  }}/bin
  export NVIM_APPNAME=lvim

  mkdir -p $HOME/.config $HOME/.data/
  nvim --headless -c 'quitall'
  ln -sfT ${astro-nvim-config} "$HOME"/.config/lvim
  if [[ -d $HOME/.data/lvim/lazy/telescope-fzf-native.nvim ]]; then
    mkdir -p "$HOME/.data/lvim/lazy/telescope-fzf-native.nvim/build"
    ln -sf "${vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so" "$HOME/.data/lvim/lazy/telescope-fzf-native.nvim/build/libfzf.so"
  fi
  if [[ -d $HOME/.data/lvim/lazy/LuaSnip/deps/jsregexp ]]; then
    ln -sf "${jsregexp}/lib/jsregexp.so" "$HOME/.data/lvim/lazy/LuaSnip/deps/jsregexp/jsregexp.so"
  fi
  exec ${neovim}/bin/nvim "$@"
''
