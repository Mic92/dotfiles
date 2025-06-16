{
  self,
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
        }
        // pkgs.lib.optionalAttrs (pkgs.stdenv.isLinux) {
          # ghostty only supports Linux platforms
          ghostty = pkgs.callPackage ./ghostty {
            ghostty = self.inputs.ghostty.packages.${system}.ghostty;
          };
        }
        // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
          sengi = pkgs.callPackage ./sengi { };
        };
    };
}
