{ stdenv, neovim-remote, wrapPython, toPythonModule }:
stdenv.mkDerivation {
  name = "nvim-open";
  unpackPhase = ":";
  nativeBuildInputs = [ wrapPython ];
  pythonPath = [ (toPythonModule neovim-remote) ];
  preFixup = "wrapPythonPrograms";
  installPhase = ''
    install -D -m755 ${./nvim-open.py} $out/bin/nvim-open
  '';
}
