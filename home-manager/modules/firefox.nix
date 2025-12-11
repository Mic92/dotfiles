{
  config,
  lib,
  pkgs,
  self,
  ...
}:

let
  system = pkgs.stdenv.hostPlatform.system;
  browserCliExtension = self.packages.${system}.browser-cli-extension;
  chromeTabGcExtension = self.packages.${system}.chrome-tab-gc-extension;
  browserCli = self.packages.${system}.browser-cli;
in
{
  # Linux only: Use home-manager Firefox configuration
  # macOS: Firefox is configured via darwinModules/firefox.nix with policies
  config = lib.mkIf pkgs.stdenv.isLinux {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox;

      # Native messaging hosts for browser-cli
      nativeMessagingHosts = [ browserCli ];

      # Firefox policies for extension installation
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

      # Default profile configuration
      profiles.default = {
        isDefault = true;

        # User preferences
        settings = {
          # Enable Wayland support
          "widget.use-xdg-desktop-portal.file-picker" = 1;

          # Privacy settings
          "browser.send_pings" = false;
          "browser.urlbar.speculativeConnect.enabled" = false;
          "dom.event.clipboardevents.enabled" = true;
          "media.navigator.enabled" = false;
          "network.cookie.cookieBehavior" = 1;
          "network.http.referer.XOriginPolicy" = 2;
          "network.http.referer.XOriginTrimmingPolicy" = 2;

          # Disable telemetry
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.ping-centre.telemetry" = false;
          "browser.tabs.crashReporting.sendReport" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.unified" = false;

          # Disable Pocket
          "extensions.pocket.enabled" = false;

          # Disable Firefox accounts
          "identity.fxaccounts.enabled" = false;

          # Performance
          "gfx.webrender.all" = true;
          "media.ffmpeg.vaapi.enabled" = true;
        };
      };
    };
  };
}
