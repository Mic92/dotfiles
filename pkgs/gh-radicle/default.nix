{
  python3,
  radicle-node,
  gh,
  git,
  rbw,
  lib,
}:

python3.pkgs.buildPythonApplication {
  pname = "gh-radicle";
  version = "0.1.0";
  pyproject = false;

  src = ./.;

  installPhase = ''
    runHook preInstall
    install -Dm755 gh_radicle.py $out/bin/gh-radicle
    runHook postInstall
  '';

  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [
      radicle-node
      gh
      git
      rbw
    ])
  ];

  meta = {
    description = "Set up automatic GitHub to Radicle mirroring";
    mainProgram = "gh-radicle";
  };
}
