{
  config,
  lib,
  ...
}:
{
  homebrew.enable = true;

  system.activationScripts.homebrew.text = lib.mkIf config.homebrew.enable (
    lib.mkBefore ''
      if [[ ! -f "${config.homebrew.brewPrefix}/brew" ]]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      fi
    ''
  );

  # Don't quarantine apps installed by homebrew with gatekeeper
  homebrew.caskArgs.no_quarantine = true;
  # Remove all homebrew packages when they get removed from the configuration
  homebrew.onActivation.cleanup = "uninstall";

  homebrew.taps = [
    "imshuhao/kdeconnect"
  ];
  homebrew.casks = [
    "kdeconnect"
    "alt-tab"
    "ferdium"
    "firefox"
    "chromium"
    "signal"
    "ghostty"
    "via"
    "claude"
  ];
}
