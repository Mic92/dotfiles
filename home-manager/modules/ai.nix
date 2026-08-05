{
  pkgs,
  lib,
  self,
  inputs,
  ...
}:
let
  aiTools = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  selfPkgs = self.packages.${pkgs.stdenv.hostPlatform.system};
  micsSkillsPkgs = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
  nixbot-cli = inputs.nixbot.packages.${pkgs.stdenv.hostPlatform.system}.nixbot-cli;

  # herdr 0.7.5 drops kitty-protocol printable keys (#1746) and shifted prefix
  # keybindings drop out of prefix mode (#1870); build master commit on Linux
  # until the next release.
  herdrPackage =
    if pkgs.stdenv.isDarwin then
      aiTools.herdr
    else
      aiTools.herdr.overrideAttrs (old: rec {
        version = "0.7.5-unstable-2026-07-29";
        src = pkgs.fetchFromGitHub {
          owner = "ogulcancelik";
          repo = "herdr";
          rev = "73d92004f50d3f5fafe64e0f9b7fddbcf4d99965";
          hash = "sha256-SeBkJePTxcFhXzFgGiSXMlAY359bgf5aTgmywZJhuoM=";
        };
        # remote.ssh_command config option; proposed upstream via discussion #1780.
        patches = (old.patches or [ ]) ++ [
          ./herdr/0001-remote-make-ssh-transport-program-configurable.patch
        ];
        cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
          inherit src;
          name = "herdr-${version}-vendor";
          hash = "sha256-Ja7fKsLWwCi6oy6zANltlFncbDVK+kgOhpr+bJtZyzg=";
        };
        # versionCheckHook compares against the pre-release Cargo version.
        doInstallCheck = false;
      });
in
{
  imports = [
    inputs.mics-skills.homeModules.default
    ./librewolf.nix
    ./herdr
  ];

  programs.herdr = {
    enable = true;
    package = herdrPackage;
    plugins = [
      selfPkgs.herdr-pluck
      selfPkgs.herdr-sesh
      selfPkgs.herdr-autoname
    ];
  };

  xdg.configFile."herdr/autoname-hook.zsh".source = "${selfPkgs.herdr-autoname}/shell/hook.zsh";

  # tmux-thumbs replacement: match sri and sha256 hashes for nix (@thumbs-regexp-1)
  xdg.configFile."herdr/plugins/config/rmarganti.herdr-pluck/config.toml".text = ''
    # OSC 52: wl-copy dies with the picker pane before the clipboard is read
    clipboard = "osc52"

    [[patterns]]
    name = "nix-hash"
    regex = '(sha256-[0-9a-zA-Z=/+]{44}|[0-9a-f]{7,40}|[0-9a-z]{52})'
  '';

  programs.mics-skills = {
    enable = true;
    package = micsSkillsPkgs // {
      # Use our msmtp wrapper that saves to Sent folder
      calendar-cli = micsSkillsPkgs.calendar-cli.override {
        msmtp = selfPkgs.msmtp-with-sent;
      };
    };
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

  # herdr's Pi integration: reports agent state/session to herdr.
  home.file.".pi/agent/extensions/herdr-agent-state.ts".source =
    "${herdrPackage.src}/src/integration/assets/pi/herdr-agent-state.ts";

  # herdr's official agent skill: spawn panes/worktrees/agents from inside a
  # herdr pane and wait for their results.
  home.file.".claude/skills/herdr/SKILL.md".source = "${herdrPackage.src}/SKILL.md";

  # nixbot-cli (nbo) ships its agent skill alongside the binary
  home.file.".claude/skills/nixbot-cli".source = "${nixbot-cli}/share/skills/nixbot-cli";

  # git-surgeon ships a skill teaching agents how to use its git primitives.
  home.file.".claude/skills/git-surgeon".source =
    "${aiTools.git-surgeon}/share/git-surgeon/skills/git-surgeon";

  # macOS-only profiler wrapper; both the skill and the binary are gated so
  # the Linux home profile doesn't pull in a darwin-only derivation.
  home.file.".claude/skills/macprof/SKILL.md" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../../pkgs/macprof/SKILL.md;
  };

  home.packages = [
    nixbot-cli
    selfPkgs.claude-code
    selfPkgs.claude-md
    selfPkgs.pim
    (pkgs.writeShellScriptBin "pi" ''
      ${pkgs.pueue}/bin/pueued -d >/dev/null 2>&1 || true
      exec ${selfPkgs.pi}/bin/pi "$@"
    '')
    aiTools.tuicr
    aiTools.openspec
    aiTools.ccstatusline
    aiTools.git-surgeon
    aiTools.jscpd
    pkgs.pueue
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
    selfPkgs.macprof
  ];
}
