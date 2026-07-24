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
  piAgentDeps = pkgs.callPackage ../../home/.pi/agent/default.nix { };
in
{
  imports = [
    inputs.mics-skills.homeModules.default
    ./librewolf.nix
    ./herdr
  ];

  programs.herdr = {
    enable = true;
    package = aiTools.herdr;
    plugins = [
      selfPkgs.herdr-pluck
      selfPkgs.herdr-sesh
      selfPkgs.herdr-autoname
    ];
  };

  xdg.configFile."herdr/autoname-hook.zsh".source = "${selfPkgs.herdr-autoname}/shell/hook.zsh";

  # tmux-thumbs replacement: match sri and sha256 hashes for nix (@thumbs-regexp-1)
  xdg.configFile."herdr/plugins/config/rmarganti.herdr-pluck/config.toml".text = ''
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
      "buildbot-pr-check"
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

  # Coordinator skill shipped by workmux upstream, installed via llm-agents.nix
  # https://github.com/numtide/llm-agents.nix/pull/3766
  home.file.".claude/skills/coordinator".source =
    "${aiTools.workmux}/share/workmux/skills/coordinator";

  # git-surgeon ships a skill teaching agents how to use its git primitives.
  home.file.".claude/skills/git-surgeon".source =
    "${aiTools.git-surgeon}/share/git-surgeon/skills/git-surgeon";

  # macOS-only profiler wrapper; both the skill and the binary are gated so
  # the Linux home profile doesn't pull in a darwin-only derivation.
  home.file.".claude/skills/macprof/SKILL.md" = lib.mkIf pkgs.stdenv.isDarwin {
    source = ../../pkgs/macprof/SKILL.md;
  };

  home.file.".claude/skills/zat/SKILL.md".text = ''
    ---
    name: zat
    description: Code outline viewer showing exported symbol signatures with line numbers. Use when you need signatures, not full implementation.
    ---

    Prefer `zat` over `cat`/`Read` when you need signatures, not full implementation. Use the line numbers in the output to `Read(offset, limit)` into specific sections.

    Supported languages: C, C++, C#, Go, Haskell, Java, JavaScript, Kotlin, Markdown, Python, Ruby, Rust, Swift, TypeScript/TSX

    ```
    zat <FILE>
    ```
  '';

  home.packages = [
    selfPkgs.claude-code
    selfPkgs.claude-md
    selfPkgs.pim
    (pkgs.writeShellScriptBin "pi" ''
      ${pkgs.pueue}/bin/pueued -d >/dev/null 2>&1 || true
      # Extensions are symlinked from dotfiles, so node walk-up misses
      # their npm deps. NODE_PATH points jiti at the prebuilt node_modules.
      export NODE_PATH="${piAgentDeps}/node_modules''${NODE_PATH:+:$NODE_PATH}"
      exec ${selfPkgs.pi}/bin/pi "$@"
    '')
    aiTools.tuicr
    aiTools.coderabbit-cli
    aiTools.openspec
    aiTools.ccusage
    aiTools.ccstatusline
    # https://github.com/Mic92/workmux/tree/fix-config-lock-race
    # Drop once upstream merges or git ships config.lock retry.
    aiTools.workmux
    aiTools.git-surgeon
    aiTools.zat
    aiTools.jscpd
    pkgs.pueue
  ]
  ++ lib.optionals pkgs.stdenv.isDarwin [
    selfPkgs.macprof
  ];
}
