{
  lib,
  rustPlatform,
}:

rustPlatform.buildRustPackage {
  pname = "herdr-autoname";
  version = "0.1.0";

  src = lib.cleanSource ./.;

  cargoLock.lockFile = ./Cargo.lock;

  # Ship as a herdr plugin directory: manifest at the root, binary under bin/,
  # zsh hook under shell/ (sourced from ~/.zshrc).
  postInstall = ''
    cp herdr-plugin.toml $out/
    install -Dm644 shell/hook.zsh $out/shell/hook.zsh
  '';

  meta = {
    description = "tmux-like automatic tab names for herdr";
    license = lib.licenses.mit;
    mainProgram = "herdr-autoname";
  };
}
