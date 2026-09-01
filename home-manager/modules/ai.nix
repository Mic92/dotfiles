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

  # Interpreter for the pi-agent-extensions python tool. Separate name so it
  # never shadows a project's python3; override per project with $PI_PYTHON.
  piPython = pkgs.python3.withPackages (ps: [
    ps.matplotlib
    ps.polars
    ps.pyelftools
    ps.requests
  ]);

  # On Darwin llm-agents ships the prebuilt release binary, so the source
  # patch can only be applied on Linux.
  herdrPackage =
    if pkgs.stdenv.hostPlatform.isDarwin then
      aiTools.herdr
    else
      aiTools.herdr.overrideAttrs (old: {
        # remote.ssh_command config option; proposed upstream via discussion #1780.
        patches = (old.patches or [ ]) ++ [
          ./herdr/0001-remote-make-ssh-transport-program-configurable.patch
        ];
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
      "queue"
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
  home.file.".claude/skills/macprof/SKILL.md" = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
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
    # interpreter for the pi-agent-extensions nushell tool
    pkgs.nushell
    (pkgs.writeShellScriptBin "pi-python" ''exec ${piPython}/bin/python3 "$@"'')
  ]
  ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
    selfPkgs.macprof
  ];
}
