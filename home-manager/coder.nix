{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  fast-nix-gc = self.inputs.fast-nix-gc.packages.${pkgs.stdenv.hostPlatform.system}.default;

  # atuin's config file wins over env vars, so swap sync_address in a
  # generated copy rather than via ATUIN_SYNC_ADDRESS.
  atuinConfig =
    builtins.replaceStrings [ "https://atuin.thalheim.io" ] [ "http://atuin-server:8080" ] (
      builtins.readFile ../home/.config/atuin/config.toml
    )
    + ''

      [daemon]
      enabled = true
    '';
  atuinConfigDir = "${config.xdg.configHome}/atuin-coder";
in
{
  imports = [
    ./common.nix
    ./modules/ai.nix
    ./modules/supervisor
  ];
  home.username = "argocd";
  home.homeDirectory = lib.mkForce "/root";

  home.sessionVariables.ATUIN_CONFIG_DIR = atuinConfigDir;
  xdg.configFile."atuin-coder/config.toml".text = atuinConfig;

  # Background sync daemon; shell client reaches it via socket.
  services.supervisor.programs.atuin-daemon.settings = {
    command = "${pkgs.atuin}/bin/atuin daemon start";
    user = "root";
    environment = ''ATUIN_CONFIG_DIR="${atuinConfigDir}"'';
  };

  home.packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.bk-wait
    pkgs.atuin
  ];

  # --- supervisor-managed services ---
  services.supervisor.enable = true;

  services.supervisor.programs.sshd = {
    settings = {
      # Use the global /sbin/sshd, not the one from nixpkgs, so it matches
      # the host PAM/NSS setup and the existing /etc/ssh/sshd_config.
      command = "/sbin/sshd -D -e";
      user = "root";
    };
    # sshd refuses to start without its privilege-separation directory,
    # and /run is tmpfs so it's gone after every workspace restart.
    preStart = ''
      mkdir -p /run/sshd
    '';
  };

  services.supervisor.programs.fast-nix-gc = {
    settings = {
      command = toString (
        pkgs.writeShellScript "fast-nix-gc-loop" ''
          while true; do
            sleep 3600
            ${fast-nix-gc}/bin/fast-nix-gc --keep-recent 1d || true
            ${fast-nix-gc}/bin/fast-nix-optimise || true
          done
        ''
      );
      user = "root";
    };
  };

  # $HOME is wiped on coder workspace restart, but ~/src persists.
  # Symlink auth/credential files from ~/src/home so logins survive restarts.
  # Use an activation script instead of home.file because tools like claude
  # code use atomic writes (write tmp + rename) which would replace a
  # home-manager managed symlink with a regular file, breaking persistence.
  home.activation.coder-persist = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    # Symlink a file into persistent storage under ~/src/home/.
    # If the file already exists as a real file, seed the persistent copy first.
    persist_link() {
      local target="$1" link="$2"
      mkdir -p "$(dirname "$link")" "$(dirname "$target")"
      if [ -f "$link" ] && [ ! -L "$link" ]; then
        if [ ! -f "$target" ]; then
          cp "$link" "$target"
        fi
        rm "$link"
      fi
      if [ ! -e "$link" ]; then
        ln -sf "$target" "$link"
      fi
      # Ensure persistent target exists (empty file) so tools don't error
      if [ ! -f "$target" ]; then
        touch "$target"
        chmod 600 "$target"
      fi
    }

    # Same idea but for directories: seed the persistent copy from any
    # existing real directory, then replace it with a symlink.
    persist_dir() {
      local target="$1" link="$2"
      mkdir -p "$(dirname "$link")"
      if [ -d "$link" ] && [ ! -L "$link" ]; then
        if [ ! -d "$target" ]; then
          mkdir -p "$(dirname "$target")"
          cp -a "$link" "$target"
        fi
        rm -rf "$link"
      fi
      mkdir -p "$target"
      if [ ! -e "$link" ]; then
        ln -sfn "$target" "$link"
      fi
    }

    # Auth credentials. ~/.pi as a whole can't be a symlink because
    # homeshick puts relative symlinks under it (settings.json) that
    # break when the directory is relocated.
    persist_link "$HOME/src/home/.pi/agent/auth.json" "$HOME/.pi/agent/auth.json"
    persist_dir "$HOME/src/home/.pi/agent/sessions" "$HOME/.pi/agent/sessions"
    persist_dir "$HOME/src/home/.pi/state" "$HOME/.pi/state"
    persist_link "$HOME/src/home/.claude/.credentials.json" "$HOME/.claude/.credentials.json"

    # atuin history DB, sync key, and session token — .zshrc runs
    # `atuin init zsh` when the binary is on PATH, so keep the store on the
    # persistent nvme or every restart loses the shell history and login.
    persist_dir "$HOME/src/home/.local/share/atuin" "$HOME/.local/share/atuin"

    # nix.conf host-specific fragment (private substituters, netrc paths).
    # Lives in persistent nvme; the main nix.conf `include`s it.
    persist_link "$HOME/src/home/.config/nix/local.conf" "$HOME/.config/nix/local.conf"

    # Coder generates ~/.gitconfig.$COO_CREATOR with work identity and
    # signing key. Homesick's .gitconfig includes ~/.gitconfig.local as the
    # per-machine override — point it at the coder-generated file so the
    # work identity wins over the personal one baked into .gitconfig.
    if [ ! -e "$HOME/.gitconfig.local" ] || [ -L "$HOME/.gitconfig.local" ]; then
      for f in "$HOME"/.gitconfig.*; do
        [ -e "$f" ] || continue
        case "$f" in
          *.local) continue ;;
        esac
        # The coder-generated file is a short [user] stub; a full
        # gitconfig with [include] would loop back through .local.
        grep -q '^\[include\]' "$f" && continue
        ln -sf "$f" "$HOME/.gitconfig.local"
        break
      done
    fi

    # On any devspace pod, authorize my GitHub-published keys so
    # agent-forwarded SSH between pods works without per-pod key setup.
    case "$(${pkgs.hostname}/bin/hostname)" in
      *-devspace-*)
        ${pkgs.curl}/bin/curl -fsSL https://github.com/Mic92.keys >> "$HOME/.ssh/authorized_keys" 2>/dev/null || true
        ${pkgs.coreutils}/bin/sort -u "$HOME/.ssh/authorized_keys" -o "$HOME/.ssh/authorized_keys"
        ;;
    esac

    # nix1 uses nix2 as a remote builder over the cluster network.
    # Auth relies on the SSH agent forwarded from the laptop, so this only
    # works for builds run from an interactive coo ssh session.
    if [ "$(${pkgs.hostname}/bin/hostname)" = "nix1-devspace-0" ]; then
      if ! grep -q '^Host nix2-builder$' "$HOME/.ssh/config" 2>/dev/null; then
        cat >> "$HOME/.ssh/config" <<'SSHEOF'

    Host nix2-builder
        HostName nix2-devspace-0.nix2-devspace.default.svc.cluster.local
        Port 2222
        User argocd
        StrictHostKeyChecking accept-new
        ServerAliveInterval 30
        ServerAliveCountMax 6
    SSHEOF
      fi

      # max-connections>1 enables nix's SSH ControlMaster (ssh-store.cc),
      # so 8 worker channels share one TCP connection.
      /usr/bin/sudo tee /etc/nix/machines > /dev/null <<'NIXEOF'
    ssh-ng://nix2-builder?max-connections=8 x86_64-linux - 192 1 big-parallel,kvm,nixos-test - -
    NIXEOF
    else
      # yensid: load-balanced x86/arm builder pools. Auth via the persistent
      # key in ~/src/keys (must be in yensid's allow-list). ssh-ng ignores
      # ssh_config Port, so the arm pool's :2222 lives in the URI.
      /usr/bin/sudo tee /etc/nix/machines > /dev/null <<'NIXEOF'
    ssh-ng://builder-ssh@x86-64-linux.yensid.rio-build.com?max-connections=4 x86_64-linux /root/src/keys/id_ed25519 64 2 benchmark,big-parallel,kvm,nixos-test - -
    ssh-ng://builder-ssh@aarch64-linux.yensid.rio-build.com:2222?max-connections=4 aarch64-linux /root/src/keys/id_ed25519 64 2 benchmark,big-parallel,kvm,nixos-test - -
    NIXEOF

      # yensid load-balances across builders with different host keys;
      # trust the CA that signs them instead of pinning each one.
      if ! /usr/bin/sudo grep -qs 'yensid.rio-build.com' /etc/ssh/ssh_known_hosts 2>/dev/null; then
        /usr/bin/sudo tee -a /etc/ssh/ssh_known_hosts > /dev/null <<'KHEOF'
    @cert-authority yensid.rio-build.com,*.yensid.rio-build.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO3TEgIFuRf18rB9tWDfNCZfprjC0hjMgSj2MTGu5jQY
    KHEOF
      fi
    fi
  '';

  # Refresh the GitHub token in nix.conf's secrets.conf each activation;
  # $HOME (and the file) is wiped on workspace restart. No gh/token: no-op.
  home.activation.nix-github-token = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if token="$(PATH="''${HM_HOST_PATH:-$PATH}" gh auth token 2>/dev/null)" && [ -n "$token" ]; then
      umask 077
      mkdir -p "$HOME/.config/nix"
      printf 'access-tokens = github.com=%s\n' "$token" > "$HOME/.config/nix/secrets.conf"
    fi
  '';
}
