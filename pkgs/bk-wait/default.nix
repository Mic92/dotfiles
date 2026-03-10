{
  lib,
  writeShellApplication,
  curl,
  python3,
}:
writeShellApplication {
  name = "bk-wait";
  runtimeInputs = [
    curl
    python3
  ];
  text = builtins.readFile ./bk-wait.sh;
  meta = {
    description = "Wait for a Buildkite build to finish by polling the REST API";
    license = lib.licenses.mit;
    mainProgram = "bk-wait";
  };
}
