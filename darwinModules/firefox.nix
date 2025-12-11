# Firefox with extensions via policies.json for macOS
#
# This module patches the nix-casks Firefox to include a distribution/policies.json
# file that automatically installs extensions.
{
  self,
  pkgs,
  lib,
  ...
}:

let
  system = pkgs.stdenv.hostPlatform.system;

  # Get extension packages
  browserCliExtension = self.packages.${system}.browser-cli-extension;
  chromeTabGcExtension = self.packages.${system}.chrome-tab-gc-extension;
  browserCli = self.packages.${system}.browser-cli;

  # Firefox policies for extension installation
  policies = {
    policies = {
      ExtensionSettings = {
        # browser-cli extension
        "browser-cli-controller@thalheim.io" = {
          installation_mode = "normal_installed";
          install_url = "file://${browserCliExtension}/browser-cli.xpi";
        };
        # Tab Garbage Collector
        "tab-garbage-collector@thalheim.io" = {
          installation_mode = "normal_installed";
          install_url = "file://${chromeTabGcExtension}/chrome-tab-gc.xpi";
        };
      };
      # Prevent auto-disabling of sideloaded extensions
      Preferences = {
        "extensions.autoDisableScopes" = {
          Value = 0;
          Status = "locked";
        };
      };
    };
  };

  policiesJson = pkgs.writeText "policies.json" (builtins.toJSON policies);

  # Patch the nix-casks Firefox to include policies.json
  firefoxCask = self.inputs.nix-casks.packages.${system}.firefox;

  firefoxWithPolicies = firefoxCask.overrideAttrs (old: {
    postInstall =
      (old.postInstall or "")
      + ''
        # Add policies.json to Firefox distribution directory
        mkdir -p "$out/Applications/Firefox.app/Contents/Resources/distribution"
        cp ${policiesJson} "$out/Applications/Firefox.app/Contents/Resources/distribution/policies.json"
      '';
  });

  # Native messaging host manifest for browser-cli
  nativeMessagingHostManifest = {
    name = "io.thalheim.browser_cli.bridge";
    description = "Browser CLI Bridge Server";
    path = "${browserCli}/bin/browser-cli-server";
    type = "stdio";
    allowed_extensions = [ "browser-cli-controller@thalheim.io" ];
  };
in
{
  # Install patched Firefox instead of plain cask
  environment.systemPackages = [ firefoxWithPolicies ];

  # Set up native messaging host for browser-cli (system-wide on macOS)
  environment.etc."mozilla/native-messaging-hosts/io.thalheim.browser_cli.bridge.json".text =
    builtins.toJSON nativeMessagingHostManifest;
}
