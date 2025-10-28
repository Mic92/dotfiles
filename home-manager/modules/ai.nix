{
  pkgs,
  self,
  inputs,
  ...
}:
{
  home.packages =
    let
      aiTools = inputs.nix-ai-tools.packages.${pkgs.system};
    in
    [
      self.packages.${pkgs.system}.claude-code
      self.packages.${pkgs.system}.pexpect-mcp
      self.packages.${pkgs.system}.claude-md
      self.packages.${pkgs.stdenv.hostPlatform.system}.kagi-search
      aiTools.opencode
      aiTools.coderabbit-cli
      aiTools.codex
      aiTools.cursor-agent
      aiTools.spec-kit
      pkgs.pueue
    ];
}
