{ stdenv, neovim-remote, makeWrapper, makePythonPath, toPythonModule }:
stdenv.mkDerivation {
  name = "nvim-open";
  unpackPhase = ":";
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    install -D -m755 ${./nvim_open/__init__.py} $out/bin/nvim-open
    wrapProgram $out/bin/nvim-open \
      --set PYTHONPATH ${makePythonPath [ (toPythonModule neovim-remote) ]}
  '';
}
