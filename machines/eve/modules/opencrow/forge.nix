{
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  config.services.opencrow.instances.forge = {
    enable = true;
    piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
    extensions = {
      memory = true;
    };
    skills =
      lib.genAttrs [
        "context7-cli"
        "pexpect-cli"
      ] (name: "${micsSkills}/skills/${name}")
      // {
        http = ./skills/http;
        gitea = ./skills/gitea;
      };
    environment = {
      TZ = "Europe/Berlin";
      OPENCROW_SOUL_FILE = "${./forge-soul.md}";
      OPENCROW_LOG_LEVEL = "info";
      OPENCROW_PI_PROVIDER = "anthropic";
      OPENCROW_PI_MODEL = "claude-opus-4-6";
      OPENCROW_NOSTR_NAME = "forge";
      OPENCROW_NOSTR_DISPLAY_NAME = "Forge";
      OPENCROW_NOSTR_ABOUT = "Your coding coworker. Code, PRs, and technical tasks.";
    };
    extraPackages = [
      micsSkillsPkgs.context7-cli
      micsSkillsPkgs.pexpect-cli
    ]
    ++ (with pkgs; [
      cacert
      coreutils
      curl
      diffutils
      fd
      findutils
      git
      gnugrep
      gnused
      jq
      patch
      python3
      ripgrep
      tree
      which
    ]);
  };
}
