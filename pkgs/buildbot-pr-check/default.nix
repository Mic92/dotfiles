{
  lib,
  buildPythonApplication,
  hatchling,
  pytest,
  vcrpy,
  pytest-vcr,
}:

buildPythonApplication {
  pname = "buildbot-pr-check";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ hatchling ];

  # No runtime dependencies, only stdlib

  nativeBuildInputs = [
    pytest
    vcrpy
    pytest-vcr
  ];

  checkPhase = ''
    runHook preCheck
    pytest tests/
    runHook postCheck
  '';

  # Skip tests by default in nix build since they require network access to record cassettes
  # Run tests with: nix build .#buildbot-pr-check --arg doCheck true
  doCheck = false;

  meta = {
    description = "Check Buildbot CI status for GitHub and Gitea pull requests";
    mainProgram = "buildbot-pr-check";
    license = lib.licenses.mit;
    maintainers = [ ];
    homepage = "https://github.com/mic92/dotfiles/tree/main/pkgs/buildbot-pr-check";
  };
}
