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
  browserCli = self.packages.${system}.browser-cli;

  # Firefox application GUID
  firefoxGuid = "{ec8030f7-c20a-464f-9b0e-13a3a9e97384}";

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
  config = lib.mkMerge [
    # Linux: Use full home-manager Firefox configuration
    (lib.mkIf pkgs.stdenv.isLinux {
      programs.firefox = {
        enable = true;
        package = pkgs.firefox;

        # Native messaging hosts for browser-cli
        nativeMessagingHosts = [ browserCli ];

        # Firefox policies for extension installation
        policies = {
          ExtensionSettings = {
            # browser-cli extension - install from local XPI
            "browser-cli-controller@thalheim.io" = {
              installation_mode = "normal_installed";
              install_url = "file://${browserCliExtension}/browser-cli.xpi";
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

            # Prevent auto-disabling of sideloaded extensions
            "extensions.autoDisableScopes" = 0;

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
    })

    # macOS: Firefox is installed via nix-casks, configure native messaging and extensions
    (lib.mkIf pkgs.stdenv.isDarwin {
      home.file = {
        # Native messaging host manifest
        "Library/Application Support/Mozilla/NativeMessagingHosts/io.thalheim.browser_cli.bridge.json".text =
          builtins.toJSON nativeMessagingHostManifest;

        # Auto-install extension via Firefox's user extensions directory
        # Using extracted folder approach (more reliable than XPI according to Mozilla docs)
        "Library/Application Support/Mozilla/Extensions/${firefoxGuid}/browser-cli-controller@thalheim.io".source =
          "${browserCliExtension}/share/mozilla/extensions/${firefoxGuid}/browser-cli-controller@thalheim.io";
      };
    })
  ];
}
