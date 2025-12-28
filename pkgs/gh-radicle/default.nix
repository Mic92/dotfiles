{
  python3,
  makeWrapper,
  radicle-node,
  gh,
  git,
}:

python3.pkgs.buildPythonApplication {
  pname = "gh-radicle";
  version = "0.1.0";
  pyproject = false;

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    install -Dm755 gh_radicle.py $out/bin/gh-radicle
    wrapProgram $out/bin/gh-radicle \
      --prefix PATH : ${radicle-node}/bin \
      --prefix PATH : ${gh}/bin \
      --prefix PATH : ${git}/bin
    runHook postInstall
  '';

  meta = {
    description = "Set up automatic GitHub to Radicle mirroring";
    mainProgram = "gh-radicle";
  };
}
