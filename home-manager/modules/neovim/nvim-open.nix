{ stdenv, neovim-remote, makeWrapper, makePythonPath, toPythonModule, python3 }:
stdenv.mkDerivation {
  name = "nvim-open";
  unpackPhase = ":";
  buildInputs = [ python3 ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    install -D -m755 ${./nvim_open/__init__.py} $out/bin/nvim-open
    patchShebangs $out/bin/nvim-open
    wrapProgram $out/bin/nvim-open \
      --set PYTHONPATH ${makePythonPath [ (toPythonModule neovim-remote) ]}
  '';
}
