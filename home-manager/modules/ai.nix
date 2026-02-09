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
      selfPkgs = self.packages.${pkgs.stdenv.hostPlatform.system};
    in
    [
      selfPkgs.claude-code
      selfPkgs.claude-md
      selfPkgs.pim
      micsSkills.context7-cli
      micsSkills.pexpect-cli
      micsSkills.kagi-search
      micsSkills.screenshot-cli
      micsSkills.db-cli
      micsSkills.gmaps-cli
      aiTools.pi
      aiTools.tuicr
      aiTools.coderabbit-cli
      aiTools.openspec
      aiTools.gemini-cli
      aiTools.ccusage
      aiTools.ccstatusline
      aiTools.workmux
      pkgs.pueue
    ];
}
