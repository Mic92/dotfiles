{
  lib,
  pkgs,
  self,
  ...
}:
{
  imports = [
    ./common.nix
    ./modules/ai.nix
    ./modules/supervisor
  ];
  home.username = "argocd";
  home.homeDirectory = lib.mkForce "/root";

  home.packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.bk-wait
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

    # Coder generates ~/.gitconfig.$COO_CREATOR with work identity and
    # signing key. Homesick's .gitconfig includes ~/.gitconfig.local as the
    # per-machine override — point it at the coder-generated file so the
    # work identity wins over the personal one baked into .gitconfig.
    if [ ! -e "$HOME/.gitconfig.local" ] || [ -L "$HOME/.gitconfig.local" ]; then
      for f in "$HOME"/.gitconfig.*; do
        [ -e "$f" ] || continue
        case "$f" in
          *.local) ;;
          *) ln -sf "$f" "$HOME/.gitconfig.local"; break ;;
        esac
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
    fi
  '';
}
