{
  python3,
  lib,
  makeWrapper,
  zenity,
}:

let
  py = python3.pkgs;
  # Patch zenity to support --text in password dialog
  zenity-patched = zenity.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./zenity-password-text-support.patch
    ];
  });
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

  propagatedBuildInputs = [
    py.keyring
  ];

  postInstall = ''
    wrapProgram $out/bin/rbw-pinentry \
      --prefix PATH : ${lib.makeBinPath [ zenity-patched ]}
  '';
}
