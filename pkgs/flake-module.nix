{
  ...
}:
{
  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        {
          merge-when-green = pkgs.callPackage ./merge-when-green { };
          claude-code = pkgs.callPackage ./claude-code { };
          gmaps-cli = pkgs.python3.pkgs.callPackage ./gmaps-cli { };
          paperless-cli = pkgs.callPackage ./paperless-cli { };
          db-cli = pkgs.callPackage ./db-cli { };
          direnv-instant = pkgs.callPackage ./direnv-instant { };
          kagi-search = pkgs.python3.pkgs.callPackage ./kagi-search { };
          email-sync = pkgs.callPackage ./email-sync { };
          vcal = pkgs.callPackage ./vcal { };
          buildbot-pr-check = pkgs.python3.pkgs.callPackage ./buildbot-pr-check { };
          claude-md = pkgs.python3.pkgs.callPackage ./claude-md { };
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
