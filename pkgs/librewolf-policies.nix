{ firefoxExtensions }:
{
  ExtensionSettings = {
    "browser-cli-controller@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = "file://${firefoxExtensions.browser-cli-extension}/browser-cli-extension.xpi";
    };
    "tab-garbage-collector@thalheim.io" = {
      installation_mode = "force_installed";
      install_url = "file://${firefoxExtensions.chrome-tab-gc-extension}/chrome-tab-gc-extension.xpi";
    };
  };
}
