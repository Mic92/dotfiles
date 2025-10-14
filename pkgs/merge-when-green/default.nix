{
  python3,
  openssh,
  gitMinimal,
  nixVersions,
  gh,
  tea,
  coreutils,
  lib,
  makeWrapper,
}:
let
  runtimeDeps = [
    gitMinimal # for git flakes
    nixVersions.latest
    coreutils
    gh # for GitHub
    tea # for Gitea
  ];
in
python3.pkgs.buildPythonApplication {
  pname = "merge-when-green";
  version = "0.3.0";
  src = ./.;
  format = "other";

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    install -D -m 0755 merge-when-green.py $out/bin/merge-when-green

    # We prefer the system's openssh over our own, since it might come with features not present in ours:
    # https://github.com/nix-community/nixos-anywhere/issues/62
    wrapProgram $out/bin/merge-when-green \
      --prefix PATH : ${lib.makeBinPath runtimeDeps} --suffix PATH : ${lib.makeBinPath [ openssh ]}
  '';

  meta = with lib; {
    description = "Merge a PR when the CI is green (supports GitHub and Gitea)";
    license = licenses.mit;
    platforms = platforms.all;
    mainProgram = "merge-when-green";
  };
}
