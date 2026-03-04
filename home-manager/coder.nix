{ lib, ... }:
{
  imports = [
    ./common.nix
    ./modules/ai.nix
  ];
  home.username = "argocd";
  home.homeDirectory = lib.mkForce "/root";

  # $HOME is wiped on coder workspace restart, but ~/src persists.
  # Symlink auth/credential files from ~/src/home so logins survive restarts.
  # Use an activation script instead of home.file because tools like claude
  # code use atomic writes (write tmp + rename) which would replace a
  # home-manager managed symlink with a regular file, breaking persistence.
  home.activation.coder-persist-auth = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    persist_link() {
      local target="$1" link="$2"
      mkdir -p "$(dirname "$link")" "$(dirname "$target")"
      if [ -f "$link" ] && [ ! -L "$link" ]; then
        # Existing real file: seed persistent storage and replace with symlink
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
    persist_link "$HOME/src/home/.pi/agent/auth.json" "$HOME/.pi/agent/auth.json"
    persist_link "$HOME/src/home/.claude/.credentials.json" "$HOME/.claude/.credentials.json"
  '';
}
