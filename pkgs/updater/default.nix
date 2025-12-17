{
  lib,
  python3,
  makeWrapper,
  nix-update,
  nix,
  git,
  gh,
}:

python3.pkgs.buildPythonApplication {
  pname = "updater";
  version = "0.1.0";
  pyproject = false;

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/updater $out/bin
    cp -r *.py $out/lib/updater/

    makeWrapper ${python3}/bin/python3 $out/bin/updater \
      --add-flags "-m updater" \
      --prefix PATH : ${
        lib.makeBinPath [
          nix-update
          nix
          git
          gh
        ]
      } \
      --set PYTHONPATH $out/lib

    runHook postInstall
  '';

  meta = {
    description = "Package updater for dotfiles";
    mainProgram = "updater";
  };
}
