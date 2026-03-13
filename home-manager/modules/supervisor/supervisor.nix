# Minimal module to manage /etc/supervisor/conf.d/ service configs.
#
# This does NOT take over the full supervisor configuration — it only
# adds .conf files for services we define.  The existing supervisord
# setup (from remote-dev) keeps running unchanged; we just drop extra
# program configs into the include directory.
#
# Config files are deployed via environment.etc (see etc.nix); this module
# only generates the content and triggers supervisorctl reload.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.supervisor;

  iniAtom = lib.types.oneOf [
    lib.types.bool
    lib.types.int
    lib.types.str
  ];

  # Wrap the command with a preStart script if one is defined.
  # Supervisor has no native ExecStartPre, so we generate a shell wrapper
  # that runs the preStart commands then exec's the real command.
  wrapCommand =
    name: svc:
    if svc.preStart == "" then
      svc.settings.command
    else
      let
        wrapper = pkgs.writeShellScript "${name}-start" ''
          ${svc.preStart}
          exec ${svc.settings.command}
        '';
      in
      "${wrapper}";

  # Generate a single [program:name] .conf file from an attrset.
  mkConf =
    name: svc:
    let
      attrs = svc.settings // {
        command = wrapCommand name svc;
      };
    in
    pkgs.writeText "${name}.conf" (lib.generators.toINI { } { "program:${name}" = attrs; });

  serviceOpts =
    { name, ... }:
    {
      options = {
        preStart = lib.mkOption {
          type = lib.types.lines;
          default = "";
          description = "Shell commands to run before the main command (wrapped via exec).";
        };

        settings = lib.mkOption {
          type = lib.types.attrsOf iniAtom;
          description = "Supervisor [program] settings written as INI key=value pairs.";
          default = { };
        };
      };

      # Sensible defaults — can be overridden per-service.
      config.settings = {
        autostart = lib.mkDefault true;
        autorestart = lib.mkDefault true;
        startsecs = lib.mkDefault 5;
        stopwaitsecs = lib.mkDefault 30;
        killasgroup = lib.mkDefault true;
        stopasgroup = lib.mkDefault true;
        stdout_logfile = lib.mkDefault "/var/log/${name}.out.log";
        stderr_logfile = lib.mkDefault "/var/log/${name}.err.log";
        stdout_logfile_maxbytes = lib.mkDefault "10MB";
        stderr_logfile_maxbytes = lib.mkDefault "10MB";
      };
    };

  supervisorctl = "${pkgs.python3Packages.supervisor}/bin/supervisorctl";
  sudo = "/usr/bin/sudo";
in
{
  options.services.supervisor = {
    enable = lib.mkEnableOption "supervisor service config management";

    programs = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule serviceOpts);
      default = { };
      description = "Supervisor programs to add to /etc/supervisor/conf.d/.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Publish each service config as an /etc entry.
    environment.etc = lib.mapAttrs' (
      name: svc:
      lib.nameValuePair "supervisor/conf.d/${name}.conf" {
        source = mkConf name svc;
      }
    ) cfg.programs;

    # Reload supervisor after etc files are deployed.
    # reread+update is cheap and idempotent — supervisor only restarts
    # programs whose config actually changed.
    home.activation.supervisor-reload = lib.hm.dag.entryAfter [ "deploy-etc" ] ''
      ${sudo} ${supervisorctl} reread
      ${sudo} ${supervisorctl} update
    '';
  };
}
