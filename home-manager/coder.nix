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
  ];
  home.username = "argocd";
  home.homeDirectory = lib.mkForce "/root";

  home.packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.iroh-ssh
  ];

  # $HOME is wiped on coder workspace restart, but ~/src persists.
  # Symlink auth/credential files from ~/src/home so logins survive restarts.
  # Use an activation script instead of home.file because tools like claude
  # code use atomic writes (write tmp + rename) which would replace a
  # home-manager managed symlink with a regular file, breaking persistence.
  home.activation.coder-persist =
    let
      iroh-ssh = self.packages.${pkgs.stdenv.hostPlatform.system}.iroh-ssh;
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
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

            # iroh-ssh identity keys (stable endpoint ID across restarts)

            # iroh-ssh remote builder config (only on fun-with-nix workspace)
            if [ "$(hostname)" = "coder-jorg-fun-with-nix-0" ]; then
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
              sudo tee /etc/nix/machines > /dev/null <<'NIXEOF'
      ssh://bld1 x86_64-linux,i686-linux /root/.ssh/iroh-builders 64 1 big-parallel - -
      ssh://bld2 x86_64-linux,i686-linux /root/.ssh/iroh-builders 64 1 big-parallel - -
      NIXEOF
            fi

            # Start iroh-ssh server daemon (idempotent — skips if already running)
            PIDFILE="$HOME/.iroh-ssh.pid"
            if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
              echo "iroh-ssh server already running (pid $(cat "$PIDFILE"))"
            else
              nohup ${iroh-ssh}/bin/iroh-ssh server --persist --ssh-port 2222 \
                >>"$HOME/.iroh-ssh.log" 2>&1 &
              echo $! > "$PIDFILE"
              echo "iroh-ssh server started (pid $!)"
            fi

            # On first boot, iroh-ssh generates keys — persist them for next restart
            if [ ! -s "$HOME/src/home/.ssh/irohssh_ed25519" ]; then
              for _ in $(seq 1 30); do
                if [ -s "$HOME/.ssh/irohssh_ed25519" ]; then
                  break
                fi
                sleep 0.2
              done
              mkdir -p "$HOME/src/home/.ssh"
              cp "$HOME/.ssh/irohssh_ed25519" "$HOME/src/home/.ssh/irohssh_ed25519"
              cp "$HOME/.ssh/irohssh_ed25519.pub" "$HOME/src/home/.ssh/irohssh_ed25519.pub"
              chmod 600 "$HOME/src/home/.ssh/irohssh_ed25519"
            fi

            persist_link "$HOME/src/home/.ssh/irohssh_ed25519" "$HOME/.ssh/irohssh_ed25519"
            persist_link "$HOME/src/home/.ssh/irohssh_ed25519.pub" "$HOME/.ssh/irohssh_ed25519.pub"
    '';
}
