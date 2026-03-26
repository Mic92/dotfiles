{
  lib,
  pkgs,
  self,
  ...
}:
let
  iroh-ssh = self.packages.${pkgs.stdenv.hostPlatform.system}.iroh-ssh;
in
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
    iroh-ssh
  ];

  # --- supervisor-managed services ---
  services.supervisor.enable = true;

  services.supervisor.programs.sshd.settings = {
    # Use the global /sbin/sshd, not the one from nixpkgs, so it matches
    # the host PAM/NSS setup and the existing /etc/ssh/sshd_config.
    command = "/sbin/sshd -D -e";
    user = "root";
  };

  services.supervisor.programs.iroh-ssh = {
    settings = {
      command = "${iroh-ssh}/bin/iroh-ssh server --persist --ssh-port 2222";
      user = 999;
      environment = "HOME=/root";
    };

    # Symlink persistent identity keys so the iroh endpoint ID survives
    # workspace restarts ($HOME is ephemeral, ~/src persists).
    preStart = ''
      PERSIST="$HOME/src/home/.ssh"
      mkdir -p "$HOME/.ssh" "$PERSIST"

      for key in irohssh_ed25519 irohssh_ed25519.pub; do
        # Seed persistent copy from any existing real file
        if [ -f "$HOME/.ssh/$key" ] && [ ! -L "$HOME/.ssh/$key" ]; then
          if [ ! -f "$PERSIST/$key" ]; then
            cp "$HOME/.ssh/$key" "$PERSIST/$key"
          fi
          rm "$HOME/.ssh/$key"
        fi
        # Create symlink if missing
        if [ ! -e "$HOME/.ssh/$key" ]; then
          ln -sf "$PERSIST/$key" "$HOME/.ssh/$key"
        fi
      done

      # Ensure persistent files exist (iroh-ssh will generate on first run
      # and write through the symlink)
      for key in irohssh_ed25519 irohssh_ed25519.pub; do
        if [ ! -f "$PERSIST/$key" ]; then
          touch "$PERSIST/$key"
          chmod 600 "$PERSIST/$key"
        fi
      done
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

    # Auth credentials
    persist_link "$HOME/src/home/.pi/agent/auth.json" "$HOME/.pi/agent/auth.json"
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

    # iroh-ssh remote builder config (only on fun-with-nix workspace)
    if [ "$(${pkgs.hostname}/bin/hostname)" = "coder-jorg-fun-with-nix-0" ]; then
      # Builder SSH key persisted across workspace restarts
      persist_link "$HOME/src/home/.ssh/iroh-builders" "$HOME/.ssh/iroh-builders"
      persist_link "$HOME/src/home/.ssh/iroh-builders.pub" "$HOME/.ssh/iroh-builders.pub"

      # SSH config for iroh-ssh remote builders (append if not already present)
      if ! grep -q 'Host bld1' "$HOME/.ssh/config" 2>/dev/null; then
        cat >> "$HOME/.ssh/config" <<'SSHEOF'

    Host bld1
        User argocd
        IdentityFile ~/.ssh/iroh-builders
        ProxyCommand iroh-ssh proxy 167e7b362ad333b19f548ce3f1de99ad220d1038900a26657e3ac069a46e0c6b

    Host bld2
        User argocd
        IdentityFile ~/.ssh/iroh-builders
        ProxyCommand iroh-ssh proxy 7d06e45b087c17e5ccc0483868a4a3d76ba59ec98ebcc989e57e946ac451850c
    SSHEOF
            fi

         # Nix remote builder machines file
         /usr/bin/sudo tee /etc/nix/machines > /dev/null <<'NIXEOF'
    ssh://bld1 x86_64-linux,i686-linux /root/.ssh/iroh-builders 64 1 big-parallel - -
    ssh://bld2 x86_64-linux,i686-linux /root/.ssh/iroh-builders 64 1 big-parallel - -
    NIXEOF
    fi

    # iroh-ssh identity keys are managed by the supervisor iroh-ssh service's
    # preStart — see services.supervisor.programs.iroh-ssh above.
  '';
}
