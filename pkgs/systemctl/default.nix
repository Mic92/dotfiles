{
  lib,
  python3,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "systemctl-macos";
  version = "1.0.0";

  src = ./.;

  nativeBuildInputs = [ python3 ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp systemctl.py $out/bin/systemctl
    chmod +x $out/bin/systemctl

    # Replace shebang with proper python3 path
    substituteInPlace $out/bin/systemctl \
      --replace "#!/usr/bin/env python3" "#!${python3}/bin/python3"

    runHook postInstall
  '';

  meta = with lib; {
    description = "systemctl compatibility layer for macOS using launchd";
    longDescription = ''
      A systemctl implementation for macOS that maps systemctl commands to their
      launchctl equivalents. Provides familiar systemctl syntax for managing
      launchd services on macOS.
    '';
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.mit;
    maintainers = with maintainers; [ mic92 ];
    platforms = platforms.darwin;
    mainProgram = "systemctl";
  };
}
