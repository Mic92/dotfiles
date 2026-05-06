{
  lib,
  python3,
  nix-eval-jobs,
  nix,
  git,
  makeWrapper,
}:

python3.pkgs.buildPythonApplication {
  pname = "minihydra";
  version = "0.1.0";

  src = ./.;
  pyproject = true;

  build-system = [ python3.pkgs.hatchling ];

  dependencies = with python3.pkgs; [
    fastapi
    uvicorn
    jinja2
    rich
    httpx
  ];

  nativeBuildInputs = [ makeWrapper ];

  nativeCheckInputs =
    with python3.pkgs;
    [
      pytestCheckHook
    ]
    ++ [
      git
      nix
      nix-eval-jobs
    ];

  postFixup = ''
    wrapProgram $out/bin/minihydra \
      --prefix PATH : ${
        lib.makeBinPath [
          nix-eval-jobs
          nix
          git
        ]
      }
  '';

  meta = {
    description = "Per-commit nixpkgs eval/build differ with web UI";
    mainProgram = "minihydra";
    license = lib.licenses.mit;
  };
}
