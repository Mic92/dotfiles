{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.krebs.secret;
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
  options.krebs.secret = {
    files = mkOption {
      type = with types; attrsOf secret-file;
      default = {};
    };
  };
  config = lib.mkIf (cfg.files != {}) {
    systemd.services.secret = let
      # TODO fail if two files have the same path but differ otherwise
      files = unique (map (flip removeAttrs ["_module"])
                          (attrValues cfg.files));
    in {
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        SyslogIdentifier = "secret";
        ExecStart = pkgs.writeScript "install-secret-files" ''
          #!${pkgs.runtimeShell}
          exit_code=0
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
            || exit_code=1
          '') files}
          exit $exit_code
        '';
      };
    };
  };
}
