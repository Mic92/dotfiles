{
  lib,
  stdenv,
  writeShellApplication,
  # Calendar tools
  khal,
  vdirsyncer,
  todoman,
  # Email tools
  notmuch,
  afew,
  mblaze,
  msmtp,
  isync,
  # Contacts
  khard,
  # Supporting tools
  rbw,
  coreutils,
  gnugrep,
  gnused,
  gawk,
  findutils,
  bash,
  util-linux,
  # Email sync
  email-sync,
  # AI tools
  pi,
  db-cli,
  kagi-search,
  # Sandboxing (Linux only)
  bubblewrap,
}:

let
  # Build a minimal PATH with only calendar/email related tools
  toolsPath = lib.makeBinPath [
    # Calendar
    khal
    vdirsyncer
    todoman
    # Email
    notmuch
    afew
    mblaze
    msmtp
    isync
    # Contacts
    khard
    # Auth
    rbw
    # Basic utils
    coreutils
    gnugrep
    gnused
    gawk
    findutils
    bash
    util-linux
    # Email sync
    email-sync
    # Skills dependencies
    db-cli
    kagi-search
  ];

  systemPrompt = ''
    You are a calendar, email, and travel planning assistant.

    Available tools:
    - Calendar: khal, vdirsyncer, todo (todoman)
    - Email: notmuch, afew, mrefile (from mblaze), msmtp, mbsync
    - Contacts: khard
    - Travel: db-cli (German trains)
    - Search: kagi-search

    Key directories:
    - Calendars: ~/.local/share/calendars/
    - Mail: ~/mail/thalheim.io/
    - Contacts: ~/.contacts/

    Common tasks:
    - List calendar events: khal list
    - List with UIDs: khal list --format "{start-time}-{end-time} {title} [{uid}]"
    - Delete calendar entry: printf "D\ny\n" | khal edit <uid>
    - List todos: todo list
    - Search email: notmuch search <query>
    - Show email: notmuch show --format=text <thread-id>
    - Sync calendars: vdirsyncer sync
    - Sync email: email-sync
    - Search contacts: khard list <name>
    - Train connections: db-cli "From" "To"
  '';

  # Directories that need read-write access (relative to $HOME)
  rwDirs = [
    ".local/share/calendars"
    ".local/share/vdirsyncer"
    ".local/share/notmuch"
    ".local/share/khal"
    ".cache/vdirsyncer"
    ".cache/khal"
    ".cache/notmuch"
    ".cache/rbw"
    ".contacts"
    "mail"
    ".pi"
    ".claude/outputs"
  ];

  # Directories/files that need read-only access (relative to $HOME)
  roDirs = [
    ".config/khal"
    ".config/vdirsyncer"
    ".config/todoman"
    ".config/notmuch"
    ".config/afew"
    ".config/msmtp"
    ".config/khard"
    ".config/rbw"
    ".mbsyncrc"
    ".notmuch-config"
    ".claude/skills"
    # Configs and extensions are symlinks to dotfiles
    ".homesick/repos/dotfiles/home"
  ];

in
writeShellApplication {
  name = "pim";

  runtimeInputs = [ pi ] ++ lib.optionals stdenv.isLinux [ bubblewrap ];

  text = ''
        TOOLS_PATH=${lib.escapeShellArg toolsPath}

        # Build system prompt with current calendar
        SYSTEM_PROMPT=${lib.escapeShellArg systemPrompt}
        SYSTEM_PROMPT+="
    Current calendar:
    $(cal -3)"

        # Ensure session directory exists
        mkdir -p "$HOME/.pi/pim"
        mkdir -p "$HOME/.claude/outputs"

        run_pi() {
            PATH="$TOOLS_PATH:$PATH" \
            pi \
                --provider anthropic \
                --model claude-haiku-4-5 \
                --session-dir "$HOME/.pi/pim" \
                --append-system-prompt "$SYSTEM_PROMPT" \
                --skills "db-cli,kagi-search" \
                "$@"
        }

        # Linux sandboxing with bwrap
        if [[ "$(uname)" == "Linux" ]] && command -v bwrap &>/dev/null; then
            # Build bwrap arguments
            BWRAP_ARGS=(
                # Basic system access
                --ro-bind /nix/store /nix/store
                --ro-bind /etc /etc
                --ro-bind /run /run
                --dev /dev
                --proc /proc
                --tmpfs /tmp

                # XDG runtime (for rbw agent socket, etc.)
                --bind "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
            )

            # Add read-write binds for data directories
            for dir in ${lib.escapeShellArgs rwDirs}; do
                target="$HOME/$dir"
                if [[ -e "$target" ]]; then
                    BWRAP_ARGS+=(--bind "$target" "$target")
                fi
            done

            # Add read-only binds for config
            for dir in ${lib.escapeShellArgs roDirs}; do
                target="$HOME/$dir"
                if [[ -e "$target" ]]; then
                    BWRAP_ARGS+=(--ro-bind "$target" "$target")
                fi
            done

            # Environment variables
            BWRAP_ARGS+=(
                --setenv HOME "$HOME"
                --setenv PATH "$TOOLS_PATH"
                --setenv TERM "''${TERM:-xterm-256color}"
                --setenv LANG "''${LANG:-en_US.UTF-8}"
                --setenv XDG_RUNTIME_DIR "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
            )

            # Pass through API keys if set
            [[ -n "''${ANTHROPIC_API_KEY:-}" ]] && BWRAP_ARGS+=(--setenv ANTHROPIC_API_KEY "$ANTHROPIC_API_KEY")
            [[ -n "''${OPENAI_API_KEY:-}" ]] && BWRAP_ARGS+=(--setenv OPENAI_API_KEY "$OPENAI_API_KEY")
            [[ -n "''${GEMINI_API_KEY:-}" ]] && BWRAP_ARGS+=(--setenv GEMINI_API_KEY "$GEMINI_API_KEY")
            [[ -n "''${KAGI_API_KEY:-}" ]] && BWRAP_ARGS+=(--setenv KAGI_API_KEY "$KAGI_API_KEY")

            # Network and namespace options
            BWRAP_ARGS+=(
                --share-net
                --unshare-pid
                --die-with-parent
            )

            exec bwrap "''${BWRAP_ARGS[@]}" \
                ${pi}/bin/pi \
                    --provider anthropic \
                    --model claude-haiku-4-5 \
                    --session-dir "$HOME/.pi/pim" \
                    --append-system-prompt "$SYSTEM_PROMPT" \
                    --skills "db-cli,kagi-search" \
                    "$@"
        else
            # macOS or no bwrap - just restrict PATH
            run_pi "$@"
        fi
  '';
}
