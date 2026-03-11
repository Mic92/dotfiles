{
  lib,
  python3,
}:
python3.pkgs.buildPythonApplication {
  pname = "bk-wait";
  version = "0.1.0";
  format = "other";

  src = ./.;

  dontBuild = true;

  installPhase = ''
    install -Dm755 bk-wait.py $out/bin/bk-wait
  '';

  meta = {
    description = "Wait for a Buildkite build to finish by polling the REST API";
    license = lib.licenses.mit;
    mainProgram = "bk-wait";
  };
}
