# Manage files under /etc via symlinks to the nix store.
#
# Inspired by nix-darwin's environment.etc — but much simpler: no /etc/static
# indirection, just direct symlinks.  Tracks which links we own via a manifest
# file so stale entries are cleaned up on the next activation.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.environment;

  fileOpts =
    { name, config, ... }:
    {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether this /etc entry should be created.";
        };

        target = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Path relative to /etc.";
        };

        source = lib.mkOption {
          type = lib.types.path;
          description = "Store path to symlink to.";
        };

        text = lib.mkOption {
          type = lib.types.nullOr lib.types.lines;
          default = null;
          description = "Text content (alternative to source).";
        };

        mode = lib.mkOption {
          type = lib.types.str;
          default = "symlink";
          description = ''
            "symlink" (default) or an octal mode like "0644" for a copy.
            Use copy mode for files that must be regular files (not symlinks).
          '';
        };
      };

      config = {
        source = lib.mkIf (config.text != null) (
          lib.mkDefault (pkgs.writeText "etc-${lib.replaceStrings [ "/" ] [ "-" ] name}" config.text)
        );
      };
    };

  enabledFiles = lib.filterAttrs (_: f: f.enable) cfg.etc;

  # Build a manifest listing all managed targets so we can clean up stale ones.
  manifest = pkgs.writeText "etc-manifest" (
    lib.concatMapStringsSep "\n" (f: f.target) (lib.attrValues enabledFiles)
  );

  deployScript = pkgs.writeShellScript "deploy-etc" ''
    manifest="${manifest}"
    manifest_state="/etc/.nix-etc-manifest"
    changed=0

    ${lib.concatMapStringsSep "\n" (
      f:
      if f.mode == "symlink" then
        ''
          # ${f.target}
          if [ "$(readlink "/etc/${f.target}" 2>/dev/null)" != "${f.source}" ]; then
            mkdir -p "$(dirname "/etc/${f.target}")"
            ln -sf "${f.source}" "/etc/${f.target}"
            changed=1
          fi
        ''
      else
        ''
          # ${f.target} (copy, mode ${f.mode})
          if ! cmp -s "${f.source}" "/etc/${f.target}" 2>/dev/null; then
            mkdir -p "$(dirname "/etc/${f.target}")"
            install -m "${f.mode}" "${f.source}" "/etc/${f.target}"
            changed=1
          fi
        ''
    ) (lib.attrValues enabledFiles)}

    # Remove stale entries from previous generations
    if [ -f "$manifest_state" ]; then
      while IFS= read -r old_target; do
        # Skip if still managed
        if grep -qxF "$old_target" "$manifest"; then
          continue
        fi
        # Only remove if it's a symlink pointing into /nix/store
        if [ -L "/etc/$old_target" ]; then
          case "$(readlink "/etc/$old_target")" in
            /nix/store/*)
              rm "/etc/$old_target"
              echo "etc: removed stale /etc/$old_target"
              changed=1
              ;;
          esac
        fi
      done < "$manifest_state"
    fi

    # Update manifest
    install -m 0644 "$manifest" "$manifest_state"

    if [ "$changed" = 1 ]; then
      echo "etc: files updated"
    else
      echo "etc: all files up to date"
    fi
  '';
in
{
  options.environment.etc = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule fileOpts);
    default = { };
    description = "Set of files to be linked/copied into /etc.";
  };

  config = lib.mkIf (enabledFiles != { }) {
    # Writing to /etc requires root.  On coder workspaces home-manager runs
    # as a non-root user (argocd), so escalate via sudo when necessary.
    home.activation.deploy-etc = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ "$(id -u)" = 0 ]; then
        run ${deployScript}
      else
        run /usr/bin/sudo ${deployScript}
      fi
    '';
  };
}
