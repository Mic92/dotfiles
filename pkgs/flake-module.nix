{
  ...
}:
{
  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        {
          mergify-gen-config = pkgs.python3.pkgs.callPackage ./mergify-gen-config { };
          merge-when-green = pkgs.callPackage ./merge-when-green { };
          gitea-mcp = pkgs.callPackage ./gitea-mcp { };
          tmux-mcp = pkgs.callPackage ./tmux-mcp { };
          claude-code = pkgs.callPackage ./claude-code { };
          gmaps-cli = pkgs.python3.pkgs.callPackage ./gmaps-cli { };
          paperless-cli = pkgs.callPackage ./paperless-cli { };
          db-cli = pkgs.callPackage ./db-cli { };
          direnv-instant = pkgs.callPackage ./direnv-instant { };
          kagi-search = pkgs.python3.pkgs.callPackage ./kagi-search { };
          email-sync = pkgs.callPackage ./email-sync { };
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
          blueutil = pkgs.callPackage ./blueutil { };
          systemctl-macos = pkgs.callPackage ./systemctl { };
        }
        // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          sengi = pkgs.callPackage ./sengi { };
        };
    };
}
