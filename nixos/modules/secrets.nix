{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.krops.secrets;
  secret-file = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        default = config._module.args.name;
      };
      path = mkOption {
        type = types.str;
        default = "/run/keys/${config.name}";
      };
      mode = mkOption {
        type = types.str;
        default = "0400";
      };
      owner = mkOption {
        type = types.str;
        default = "root";
      };
      group-name = mkOption {
        type = types.str;
        default = "root";
      };
      source-path = mkOption {
        type = types.str;
        default = toString <secrets> + "/${config.name}";
      };
    };
  });
in {
  options.krops.secrets = {
    files = mkOption {
      type = with types; attrsOf secret-file;
      default = {};
    };
  };
  config = lib.mkIf (cfg.files != {}) {
    system.activationScripts.setup-secrets = let
      files = unique (map (flip removeAttrs ["_module"])
                          (attrValues cfg.files));
      script = ''
        echo setting up secrets...
        mkdir -p /run/keys -m 0750
        chown root:keys /run/keys
        ${concatMapStringsSep "\n" (file: ''
          ${pkgs.coreutils}/bin/install \
            -D \
            --compare \
            --verbose \
            --mode=${lib.escapeShellArg file.mode} \
            --owner=${lib.escapeShellArg file.owner} \
            --group=${lib.escapeShellArg file.group-name} \
            ${lib.escapeShellArg file.source-path} \
            ${lib.escapeShellArg file.path} \
          || echo "failed to copy ${file.source-path} to ${file.path}"
        '') files}
      '';
    in stringAfter [ "users" "groups" ] "source ${pkgs.writeText "setup-secrets.sh" script}";
  };
}
