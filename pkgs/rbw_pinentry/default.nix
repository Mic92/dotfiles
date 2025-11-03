{
  python3,
  stdenv,
  lib,
  makeWrapper,
  zenity,
}:

let
  py = python3.pkgs;
in
py.buildPythonApplication {
  pname = "rbw-pinentry";
  version = "0.1.0";
  format = "pyproject";

  src = ./.;

  nativeBuildInputs = [
    py.hatchling
    makeWrapper
  ];

  propagatedBuildInputs = lib.optionals (!stdenv.isDarwin) [
    py.secretstorage
  ];

  postInstall = ''
    wrapProgram $out/bin/rbw-pinentry \
      --prefix PATH : ${lib.makeBinPath [ zenity ]}
  '';
}
