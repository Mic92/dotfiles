{ stdenv
, openssh
, gitMinimal
, nixVersions
, gh
, coreutils
, lib
, makeWrapper
}:
let
  runtimeDeps = [
    gitMinimal # for git flakes
    nixVersions.latest
    coreutils
    gh
  ];
in
stdenv.mkDerivation {
  pname = "merge-when-green";
  version = "0.1.0";
  src = ./.;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    install -D -m 0755 script.sh $out/bin/merge-when-green

    # We prefer the system's openssh over our own, since it might come with features not present in ours:
    # https://github.com/nix-community/nixos-anywhere/issues/62
    wrapProgram $out/bin/merge-when-green \
      --prefix PATH : ${lib.makeBinPath runtimeDeps} --suffix PATH : ${lib.makeBinPath [ openssh ]}
  '';

  meta = with lib; {
    description = "Merge a PR when the CI is green";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
