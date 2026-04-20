{
  lib,
  stdenv,
  browser-cli-extension,
  chrome-tab-gc-extension,
}:
{
  ExtensionSettings = {
    "browser-cli-controller@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = "file://${browser-cli-extension}/browser-cli-extension.xpi";
    };
    "tab-garbage-collector@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = "file://${chrome-tab-gc-extension}/chrome-tab-gc-extension.xpi";
    };
  }
  // lib.optionalAttrs (!stdenv.hostPlatform.isDarwin) {
    # Live-themes the browser chrome from noctalia's generated palette so
    # LibreWolf matches the niri/bar colours. The noctalia/pywal pipeline only
    # exists on Linux, so on macOS the addon would just complain about a
    # missing native messaging host.
    "pywalfox@frewacom.org" = {
      installation_mode = "force_installed";
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/pywalfox/latest.xpi";
    };
  };
}
