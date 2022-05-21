{self, ...}: {
  perSystem = system: {
    config,
    self',
    inputs',
    pkgs,
    ...
  }: let
    poetry2nix = pkgs.callPackage self.inputs.poetry2nix {};
    poetryPackages =
      (poetry2nix.mkPoetryPackages {
        projectDir = ./.;
        overrides = poetry2nix.overrides.withDefaults (self: super: {
          xontrib-dracula = super.xontrib-dracula.overridePythonAttrs (
            old: {
              nativeBuildInputs = [super.poetry-core];
            }
          );
        });
      })
      .poetryPackages;
    extensionsEnv = pkgs.python3.buildEnv.override {
      extraLibs = poetryPackages;
    };
    pythonPath = pkgs.lib.makeSearchPathOutput "lib" pkgs.python3.sitePackages [extensionsEnv];
  in {
    packages.xonsh = pkgs.runCommand "xonsh" {nativeBuildInputs = [pkgs.buildPackages.makeWrapper];} ''
      makeWrapper ${pkgs.xonsh}/bin/xonsh $out/bin/xonsh \
        --prefix PATH : "${pkgs.lib.makeBinPath [pkgs.starship pkgs.fzf pkgs.zoxide]}" \
        --prefix PYTHONPATH : "${pythonPath}" \
        --add-flags "--rc ${./.}/xonsh.rc"
    '';
    packages.default = self'.packages.xonsh;
  };
}
