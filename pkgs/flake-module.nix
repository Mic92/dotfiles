{
  ...
}:
{
  perSystem =
    {
      inputs',
      pkgs,
      config,
      ...
    }:
    {
      packages = {
        merge-when-green = pkgs.callPackage ./merge-when-green { };
        claude-code = pkgs.callPackage ./claude-code {
          claude-code = inputs'.nix-ai-tools.packages.claude-code;
          inherit (config.packages) pexpect-mcp;
        };
        gmaps-cli = pkgs.python3.pkgs.callPackage ./gmaps-cli { };
        db-cli = pkgs.callPackage ./db-cli { };
        kagi-search = pkgs.python3.pkgs.callPackage ./kagi-search { };
        email-sync = pkgs.callPackage ./email-sync { };
        vcal = pkgs.callPackage ./vcal { };
        buildbot-pr-check = pkgs.python3.pkgs.callPackage ./buildbot-pr-check { };
        claude-md = pkgs.python3.pkgs.callPackage ./claude-md { };
        browser-cli = pkgs.python3.pkgs.callPackage ./browser-cli { };
        pexpect-mcp = pkgs.python3.pkgs.callPackage ./pexpect-mcp { };
        iroh-ssh = pkgs.callPackage ./iroh-ssh { };
        phpldapadmin = pkgs.callPackage ../nixosModules/phpldapadmin/package.nix { };
        # Cross-platform secure pinentry (works on macOS and Linux)
        rbw-pinentry = pkgs.callPackage ./rbw_pinentry { };
      }
      // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        blueutil = pkgs.callPackage ./blueutil { };
        systemctl-macos = pkgs.callPackage ./systemctl { };
      }
      // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
        sengi = pkgs.callPackage ./sengi { };
        cewe-fotowelt = pkgs.callPackage ./cewe-fotowelt { };
      };
    };
}
