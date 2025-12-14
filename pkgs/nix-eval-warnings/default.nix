{
  lib,
  python3,
  nix-eval-jobs,
  nix,
  git,
  makeWrapper,
}:

python3.pkgs.buildPythonApplication {
  pname = "nix-eval-warnings";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ python3.pkgs.hatchling ];

  nativeBuildInputs = [ makeWrapper ];

  nativeCheckInputs = [
    python3.pkgs.pytestCheckHook
    git
    nix
    nix-eval-jobs
  ];

  postFixup = ''
    wrapProgram $out/bin/nix-eval-warnings \
      --prefix PATH : ${lib.makeBinPath [ nix-eval-jobs ]}
  '';

  meta = {
    description = "Tool to extract nix evaluation warnings with stack traces";
    mainProgram = "nix-eval-warnings";
    license = lib.licenses.mit;
  };
}
