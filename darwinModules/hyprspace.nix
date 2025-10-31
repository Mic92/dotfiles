{
  config,
  lib,
  options,
  pkgs,
  self,
  ...
}:

let
  inherit (lib)
    types
    mkOption
    mkEnableOption
    mkIf
    escapeShellArg
    ;

  cfg = config.services.hyprspace;

  usePrivateKeyFromFile = options.services.hyprspace.privateKeyFile.isDefined;
  privKeyMarker = "@HYPRSPACEPRIVATEKEY@";
  runConfig = "/var/run/hyprspace.json";
  configFile = pkgs.writeText "hyprspace-config.json" (
    builtins.toJSON (
      cfg.settings
      // {
        privateKey = if usePrivateKeyFromFile then privKeyMarker else cfg.settings.privateKey;
      }
    )
  );
in

{
  options.services.hyprspace = {
    enable = mkEnableOption "Hyprspace";

    package = mkOption {
      type = types.package;
      description = "Hyprspace package to use.";
      default = self.inputs.hyprspace.packages.${pkgs.stdenv.hostPlatform.system}.hyprspace;
    };

    interface = mkOption {
      type = types.str;
      description = "Interface name.";
      default = "hyprspace";
    };

    settings = mkOption {
      type = types.submodule (import "${self.inputs.hyprspace}/nixos/settings.nix");
      description = "Hyprspace configuration options.";
      default = { };
    };

    privateKeyFile = mkOption {
      type = types.path;
      description = "File containing this node's private key.";
    };
  };

  config = mkIf cfg.enable {
    launchd.daemons.hyprspace = {
      script =
        (
          if usePrivateKeyFromFile then
            ''
              test -e ${runConfig} && rm -f ${runConfig}
              cp ${configFile} ${runConfig}
              chmod 0600 ${runConfig}
              ${lib.getExe pkgs.replace-secret} '${privKeyMarker}' "${cfg.privateKeyFile}" ${runConfig}
              chmod 0400 ${runConfig}
            ''
          else
            ""
        )
        + ''
          exec ${lib.getExe cfg.package} up -c ${
            if usePrivateKeyFromFile then runConfig else configFile
          } -i ${escapeShellArg cfg.interface}
        '';

      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        StandardErrorPath = "/var/log/hyprspace.log";
        StandardOutPath = "/var/log/hyprspace.log";
      };
    };

    environment.systemPackages = [ cfg.package ];

    # Clan vars integration
    clan.core.vars.generators.hyprspace = {
      files.private-key = { };
      files.peer-id = {
        deploy = false;
        secret = false;
      };
      runtimeInputs = [
        cfg.package
        pkgs.jq
      ];
      script = ''
        ${lib.getExe cfg.package} init -c $out/hyprspace.json | tail -n+3 | jq '.name = "${config.clan.core.settings.machine.name}"' > $out/peer-id
        jq -r '.privateKey' < $out/hyprspace.json > $out/private-key
      '';
    };

    services.hyprspace = {
      # To get a private key and peer ID, use `hyprspace init`
      privateKeyFile = config.clan.core.vars.generators.hyprspace.files.private-key.path;

      # Read peers from clan vars
      settings.peers =
        let
          varDir = "${config.clan.core.settings.directory}/vars/per-machine";
          machineDirs = if builtins.pathExists varDir then builtins.readDir varDir else { };
          peers = lib.filterAttrs (
            name: type:
            type == "directory"
            && name != config.clan.core.settings.machine.name
            && builtins.pathExists "${varDir}/${name}/hyprspace/peer-id/value"
          ) machineDirs;
        in
        lib.mapAttrsToList (
          name: _: builtins.fromJSON (builtins.readFile "${varDir}/${name}/hyprspace/peer-id/value")
        ) peers;
    };
  };
}
