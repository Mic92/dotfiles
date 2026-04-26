{
  python3,
  lib,
  stdenv,
  makeWrapper,
  zenity,
}:

let
  py = python3.pkgs;
  # On darwin we use the system osascript dialog instead of zenity, so the
  # zenity dependency (and its currently broken appstream closure) is not
  # needed there.
  useZenity = !stdenv.hostPlatform.isDarwin;
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

  postInstall = lib.optionalString useZenity ''
    wrapProgram $out/bin/rbw-pinentry \
      --prefix PATH : ${lib.makeBinPath [ zenity-patched ]}
  '';
}
