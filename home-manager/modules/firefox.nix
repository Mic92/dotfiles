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
          # === Linux Desktop Integration ===
          # Use XDG desktop portal for file picker (native Wayland/GTK dialogs)
          "widget.use-xdg-desktop-portal.file-picker" = 1;

          # === Privacy: Tracking Prevention ===
          # Disable hyperlink ping tracking (prevents <a ping="url"> from sending POST requests)
          "browser.send_pings" = false;

          # Disable speculative connections to sites when hovering over links
          # (prevents DNS/TCP/TLS connections before you actually click)
          "browser.urlbar.speculativeConnect.enabled" = false;

          # Disable websites from detecting clipboard copy/cut/paste events
          # Note: kept enabled (true) as many web apps legitimately need this
          "dom.event.clipboardevents.enabled" = true;

          # Disable websites from accessing camera/microphone status
          # (prevents fingerprinting via media device enumeration)
          "media.navigator.enabled" = false;

          # === Privacy: Cookie Policy ===
          # 0=all cookies, 1=only from originating site, 2=none, 3=from visited, 4=new cookie jar per site
          # Setting 1: Block third-party cookies (good balance of privacy vs compatibility)
          "network.cookie.cookieBehavior" = 1;

          # === Privacy: Referer Header Control ===
          # Controls when to send Referer header cross-origin:
          # 0=always send, 1=send if base domains match, 2=send only if hosts match
          # Setting 2: Only send referer to same host (prevents cross-site tracking)
          "network.http.referer.XOriginPolicy" = 2;

          # Controls how much of the URL to include in cross-origin Referer:
          # 0=full URL, 1=without query string, 2=only origin (scheme+host+port)
          # Setting 2: Only send origin (hides path/query from other sites)
          "network.http.referer.XOriginTrimmingPolicy" = 2;

          # === Telemetry: Disable Mozilla Data Collection ===
          # Disable telemetry from Firefox's new tab page activity stream
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;

          # Disable Mozilla's ping centre telemetry
          "browser.ping-centre.telemetry" = false;

          # Don't automatically send crash reports to Mozilla
          "browser.tabs.crashReporting.sendReport" = false;

          # Disable the telemetry system entirely
          "toolkit.telemetry.enabled" = false;

          # Clear telemetry server URL (belt and suspenders)
          "toolkit.telemetry.server" = "";

          # Disable unified telemetry (combines multiple telemetry types)
          "toolkit.telemetry.unified" = false;

          # === Disable Mozilla Services ===
          # Disable Pocket integration (Mozilla's read-it-later service)
          "extensions.pocket.enabled" = false;

          # Disable Firefox Accounts integration (sync, etc.)
          "identity.fxaccounts.enabled" = false;

          # === Performance ===
          # Enable WebRender compositor for GPU-accelerated rendering
          "gfx.webrender.all" = true;

          # Enable VA-API hardware video decoding on Linux
          "media.ffmpeg.vaapi.enabled" = true;
        };
      };
    };
  };
}
