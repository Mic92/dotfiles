{
  config,
  lib,
  ...
}:
let
  configuredCasks = builtins.toFile "new-casks" (
    lib.concatMapStringsSep "\n" (cask: cask.name) config.homebrew.casks
  );
in
{
  homebrew.enable = true;

  system.activationScripts.homebrew.text = lib.mkIf config.homebrew.enable (
    lib.mkBefore ''
      if [[ -f "${config.homebrew.brewPrefix}/brew" ]]; then
        comm -23 \
          <(${config.homebrew.brewPrefix}/brew list --casks -q -1 | sort -u) \
          <(sort -u ${configuredCasks}) | xargs --no-run-if-empty -I {} ${config.homebrew.brewPrefix}/brew uninstall --cask {}
      else
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      fi
    ''
  );
  homebrew.casks = [
    "amethyst"
    "ferdium"
    "firefox"
    "signal"
  ];
}
