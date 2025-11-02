{
  python3,
  stdenv,
  lib,
  makeWrapper,
  pinentry-curses,
  pinentry_mac,
  pinentry-qt,
}:

let
  # Create a Python environment with required dependencies
  pythonEnv = python3.withPackages (
    ps:
    with ps;
    [
      # secretstorage is needed for non-Darwin platforms (Linux, BSD, etc.)
    ]
    ++ lib.optionals (!stdenv.isDarwin) [
      ps.secretstorage
    ]
  );

  # Select the appropriate pinentry program based on platform
  pinentryProgram = if stdenv.isDarwin then pinentry_mac else pinentry-qt;

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

    # Wrap the script to ensure pinentry is in PATH
    wrapProgram $out/bin/rbw-pinentry \
      --prefix PATH : ${
        lib.makeBinPath [
          pinentryProgram
          pinentry-curses
        ]
      }
  '';
}
