{
  pkgs,
  self,
  inputs,
  lib,
  ...
}:
{
  home.packages =
    let
      aiTools = inputs.nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system};
    in
    [
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
      self.packages.${pkgs.stdenv.hostPlatform.system}.pexpect-cli
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-md
      self.packages.${pkgs.stdenv.hostPlatform.system}.kagi-search
      aiTools.coderabbit-cli
      aiTools.codex
      aiTools.cursor-agent
      aiTools.spec-kit
      aiTools.gemini-cli
      aiTools.ccusage
      aiTools.ccstatusline
      pkgs.pueue
    ]
    # Temporarily exclude opencode on macOS due to build timeout issues
    # See: https://github.com/Mic92/dotfiles/pull/3971
    ++ lib.optionals (!pkgs.stdenv.isDarwin) [ aiTools.opencode ];
}
