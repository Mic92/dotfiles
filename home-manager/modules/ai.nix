{
  pkgs,
  self,
  inputs,
  ...
}:
{
  home.file.".claude/skills".source = "${inputs.mics-skills}/skills";

  home.packages =
    let
      aiTools = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
      micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
    in
    [
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
      micsSkills.pexpect-cli
      self.packages.${pkgs.stdenv.hostPlatform.system}.claude-md
      micsSkills.kagi-search
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
