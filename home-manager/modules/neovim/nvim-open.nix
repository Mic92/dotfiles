{
  stdenv,
  neovim-remote,
  makeWrapper,
  makePythonPath,
  toPythonModule,
  python,
}:
stdenv.mkDerivation {
  name = "nvim-open";
  unpackPhase = ":";
  buildInputs = [ python ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    install -D -m755 ${./nvim_open.py} $out/bin/nvim-open
    patchShebangs $out/bin/nvim-open
    wrapProgram $out/bin/nvim-open \
      --set PYTHONPATH ${makePythonPath [ (toPythonModule neovim-remote) ]}
  '';
}
