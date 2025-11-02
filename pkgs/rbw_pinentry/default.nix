{
  python3,
  stdenv,
  lib,
  makeWrapper,
  zenity,
}:

let
  # Create a Python environment with required dependencies
  pythonEnv = python3.withPackages (
    ps:
    lib.optionals (!stdenv.isDarwin) [
      ps.secretstorage
    ]
  );

  pinentryScript = ./pinentry_keychain.py;
in
stdenv.mkDerivation {
  pname = "rbw-pinentry";
  version = "0.1.0";

  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cat > $out/bin/rbw-pinentry << 'EOF'
    #!${pythonEnv}/bin/python3
    ${builtins.readFile pinentryScript}
    EOF
    chmod +x $out/bin/rbw-pinentry

    # Wrap the script to ensure zenity is in PATH
    wrapProgram $out/bin/rbw-pinentry \
      --prefix PATH : ${
        lib.makeBinPath [
          zenity
        ]
      }
  '';
}
