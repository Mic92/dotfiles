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
      if [[ ! -f "${config.homebrew.brewPrefix}/brew" ]]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      fi
      comm -23 \
        <(/opt/homebrew/bin/brew list --casks -q -1 | sort -u) \
        <(sort -u ${configuredCasks}) | xargs --no-run-if-empty -I {} /opt/homebrew/bin/brew uninstall --cask {}
    ''
  );
  homebrew.casks = [
    "amethyst"
    "ferdium"
    "firefox"
    "signal"
  ];
}
