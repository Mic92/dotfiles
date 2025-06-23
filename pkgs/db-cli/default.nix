{
  lib,
  buildNpmPackage,
  importNpmLock,
}:

let
  src = ./.;
in

buildNpmPackage {
  pname = "db-cli";
  version = "1.0.0";

  inherit src;
  npmDeps = importNpmLock { npmRoot = src; };

  npmConfigHook = importNpmLock.npmConfigHook;

  # Don't install devDependencies
  npmInstallFlags = [ "--production" ];

  # The install phase will handle creating the wrapper script
  dontNpmBuild = true;

  meta = with lib; {
    description = "CLI tool for searching Deutsche Bahn train connections";
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.isc;
    maintainers = with maintainers; [ mic92 ];
    mainProgram = "db-cli";
  };
}
