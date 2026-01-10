{
  pkgs,
  self,
  inputs,
  ...
}:
{
  home.packages =
    let
      aiTools = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
    in
    [
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
      self.packages.${pkgs.stdenv.hostPlatform.system}.pexpect-cli
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-md
      self.packages.${pkgs.stdenv.hostPlatform.system}.kagi-search
      aiTools.pi
      aiTools.tuicr
      aiTools.coderabbit-cli
      aiTools.spec-kit
      aiTools.gemini-cli
      aiTools.ccusage
      aiTools.ccstatusline
      aiTools.coding-agent-search
      pkgs.pueue
    ];
}
