{
  python3,
  makeWrapper,
  git,
  radicle-node,
}:
python3.pkgs.buildPythonApplication {
  pname = "radicle-github-sync";
  version = "0.1.0";
  format = "other";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    install -Dm755 radicle_github_sync.py $out/bin/radicle-github-sync
    wrapProgram $out/bin/radicle-github-sync \
      --prefix PATH : ${git}/bin:${radicle-node}/bin
  '';

  meta.mainProgram = "radicle-github-sync";
}
