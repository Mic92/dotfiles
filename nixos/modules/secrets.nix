{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.krops.secrets;
  users = config.users.users;
  secret-file = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        default = config._module.args.name;
      };
      path = assert lib.assertMsg (builtins.pathExists config.sourcePath) ''
          Cannot find path '${config.sourcePath}' set in 'krops.secrets."${config._module.args.name}".sourcePath'
        '';
        mkOption {
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
      group = mkOption {
        type = types.str;
        default = users.${config.owner}.group;
      };
      sourcePath = mkOption {
        type = types.str;
        default = (toString <secrets> + "/${config.name}");
      };
    };
  });
in {
  options.krops.secrets = mkOption {
    type = with types; attrsOf secret-file;
    default = {};
  };
  config = lib.mkIf (cfg != {}) {
    system.activationScripts.setup-secrets = let
      files = unique (map (flip removeAttrs ["_module"])
                          (attrValues cfg));
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
            --group=${lib.escapeShellArg file.group} \
            ${lib.escapeShellArg file.sourcePath} \
            ${lib.escapeShellArg file.path} \
          || echo "failed to copy ${file.sourcePath} to ${file.path}"
        '') files}
      '';
    in stringAfter [ "users" "groups" ] "source ${pkgs.writeText "setup-secrets.sh" script}";
  };
}
