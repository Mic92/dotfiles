{
  pkgs,
  lib,
  inputs',
  self',
  self,
  system,
  ...
}:
let
  inputs = self.inputs;
  micsSkills = inputs'.mics-skills;
  aiTools = inputs'.llm-agents;
  bun2nix = inputs.llm-agents.inputs.bun2nix.packages.${system}.default;
in
{
  packages = {
    forge-triage = inputs'.forge-triage.default;
    merge-when-green = pkgs.callPackage ./merge-when-green {
      flake-fmt = inputs'.flake-fmt.default;
    };
    claude-code = pkgs.callPackage ./claude-code {
      claude-code = inputs'.llm-agents.claude-code;
    };
    n8n-cli = pkgs.callPackage ./n8n-cli { inherit bun2nix; };

    email-sync = pkgs.callPackage ./email-sync { };
    vcal = pkgs.callPackage ./vcal { };
    buildbot-pr-check = pkgs.python3.pkgs.callPackage ./buildbot-pr-check { };
    claude-md = pkgs.python3.pkgs.callPackage ./claude-md { };
    crabfit-cli = pkgs.python3.pkgs.callPackage ./crabfit-cli { };
    inherit (pkgs.callPackages ./firefox-extensions { })
      chrome-tab-gc-extension
      ;
    gh-radicle = pkgs.callPackage ./gh-radicle { };
    iroh-ssh = pkgs.callPackage ./iroh-ssh { };
    # Cross-platform secure pinentry (works on macOS and Linux)
    rbw-pinentry = pkgs.callPackage ./rbw_pinentry { };
    # Matrix calendar bot
    calendar-bot = pkgs.python3.pkgs.callPackage ./calendar_bot { };
    # Nix evaluation warnings extractor
    nix-eval-warnings = pkgs.callPackage ./nix-eval-warnings { };
    # Reference all flake inputs to ensure they get cached
    flake-inputs = pkgs.callPackage ./flake-inputs { inherit inputs; };
    # Package updater CLI
    kimai-cli = pkgs.callPackage ./kimai-cli { };
    sediment = pkgs.callPackage ./sediment { };
    updater = pkgs.callPackage ./updater { };
    # Sandboxed pi for calendar/email tasks
    pim = pkgs.callPackage ./pim {
      inherit (self'.packages) email-sync crabfit-cli;
      pi = aiTools.pi;
      db-cli = micsSkills.db-cli;
      kagi-search = micsSkills.kagi-search;
    };
  }
  // lib.optionalAttrs pkgs.stdenv.isDarwin {
    alertmanager-bar = pkgs.callPackage ./alertmanager-bar { };
    blueutil = pkgs.callPackage ./blueutil { };
    systemctl-macos = pkgs.callPackage ./systemctl { };
  }
  // lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "aarch64-darwin") {
    kdeconnect = pkgs.callPackage ./kdeconnect { };
    librewolf-macos = pkgs.callPackage ./librewolf-macos { };
    radicle-desktop = pkgs.callPackage ./radicle-desktop { };
  }
  // lib.optionalAttrs pkgs.stdenv.isLinux {
    groups-relay = pkgs.callPackage ./groups-relay { };
    phantun = pkgs.callPackage ./phantun { };
    phpldapadmin = pkgs.callPackage ../nixosModules/phpldapadmin/package.nix { };
    radicle-github-sync = pkgs.callPackage ./radicle-github-sync { };
  }
  // lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
    cewe-fotowelt = pkgs.callPackage ./cewe-fotowelt { };
  };
}
