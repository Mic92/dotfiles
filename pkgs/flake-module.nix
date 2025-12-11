{
  ...
}:
{
  perSystem =
    {
      inputs',
      pkgs,
      ...
    }:
    {
      packages = {
        merge-when-green = pkgs.callPackage ./merge-when-green { };
        claude-code = pkgs.callPackage ./claude-code {
          claude-code = inputs'.llm-agents.packages.claude-code;
        };
        gmaps-cli = pkgs.python3.pkgs.callPackage ./gmaps-cli { };
        db-cli = pkgs.callPackage ./db-cli { };
        kagi-search = pkgs.python3.pkgs.callPackage ./kagi-search { };
        email-sync = pkgs.callPackage ./email-sync { };
        vcal = pkgs.callPackage ./vcal { };
        buildbot-pr-check = pkgs.python3.pkgs.callPackage ./buildbot-pr-check { };
        claude-md = pkgs.python3.pkgs.callPackage ./claude-md { };
        browser-cli = pkgs.python3.pkgs.callPackage ./browser-cli { };
        inherit (pkgs.callPackages ./firefox-extensions { })
          browser-cli-extension
          chrome-tab-gc-extension
          ;
        pexpect-cli = pkgs.callPackage ./pexpect-cli { };
        iroh-ssh = pkgs.callPackage ./iroh-ssh { };
        # Cross-platform secure pinentry (works on macOS and Linux)
        rbw-pinentry = pkgs.callPackage ./rbw_pinentry { };
        # Matrix calendar bot
        calendar-bot = pkgs.python3.pkgs.callPackage ./calendar_bot { };
      }
      // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        blueutil = pkgs.callPackage ./blueutil { };
        systemctl-macos = pkgs.callPackage ./systemctl { };
      }
      // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") {
        kdeconnect = pkgs.callPackage ./kdeconnect { };
        librewolf-macos = pkgs.callPackage ./librewolf-macos { };
      }
      // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
        phantun = pkgs.callPackage ./phantun { };
        phpldapadmin = pkgs.callPackage ../nixosModules/phpldapadmin/package.nix { };
      }
      // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
        cewe-fotowelt = pkgs.callPackage ./cewe-fotowelt { };
      };
    };
}
