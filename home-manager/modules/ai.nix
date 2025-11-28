{
  pkgs,
  self,
  inputs,
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
      aiTools.opencode
      aiTools.coderabbit-cli
      aiTools.codex
      aiTools.cursor-agent
      aiTools.spec-kit
      aiTools.gemini-cli
      aiTools.ccusage
      aiTools.ccstatusline
      pkgs.pueue
    ];
}
