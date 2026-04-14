{
  lib,
  python3,
}:
python3.pkgs.buildPythonApplication {
  pname = "macprof";
  version = "0.1.0";
  format = "other";

  src = ./.;

  dontBuild = true;

  installPhase = ''
    install -Dm755 macprof.py $out/bin/macprof
  '';

  meta = {
    description = "Wrap macOS sample(1)/spindump(8) and emit folded or speedscope profiles";
    license = lib.licenses.mit;
    platforms = lib.platforms.darwin;
    mainProgram = "macprof";
  };
}
