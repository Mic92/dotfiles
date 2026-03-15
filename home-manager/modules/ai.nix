{
  pkgs,
  self,
  inputs,
  ...
}:
let
  aiTools = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  selfPkgs = self.packages.${pkgs.stdenv.hostPlatform.system};
  micsSkillsPkgs = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  imports = [
    inputs.mics-skills.homeManagerModules.default
    ./librewolf.nix
  ];

  programs.mics-skills = {
    enable = true;
    package = micsSkillsPkgs // {
      # Use our msmtp wrapper that saves to Sent folder
      calendar-cli = micsSkillsPkgs.calendar-cli.override {
        msmtp = selfPkgs.msmtp-with-sent;
      };
    };
    skillsSrc = inputs.mics-skills;
    skills = [
      "browser-cli"
      "calendar-cli"
      "context7-cli"
      "db-cli"
      "gmaps-cli"
      "kagi-search"
      "n8n-cli"
      "pexpect-cli"
      "screenshot-cli"
    ];
  };

  home.packages = [
    selfPkgs.claude-code
    selfPkgs.claude-md
    selfPkgs.pim
    (pkgs.writeShellScriptBin "pi" ''
      ${pkgs.pueue}/bin/pueued -d 2>/dev/null || true
      exec ${aiTools.pi}/bin/pi "$@"
    '')
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
