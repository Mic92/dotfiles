{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    inputs.hyprspace.nixosModules.default
  ];

  services.hyprspace = {
    enable = true;

    # To get a private key and peer ID, use `hyprspace init`
    privateKeyFile = config.clan.core.vars.generators.hyprspace.files.private-key.path;

    # Same as the config file
    settings.peers =
      let
        machineDirs = builtins.readDir "${config.clan.core.settings.directory}/vars/per-machine";
        peers = lib.filterAttrs (
          name: type:
          type == "directory"
          && name != config.clan.core.settings.machine.name
          && builtins.pathExists "${config.clan.core.settings.directory}/vars/per-machine/${name}/hyprspace/peer-id/value"

        ) machineDirs;
      in
      lib.mapAttrsToList (
        name: _:
        builtins.fromJSON (
          builtins.readFile "${config.clan.core.settings.directory}/vars/per-machine/${name}/hyprspace/peer-id/value"
        )
      ) peers;
  };

  networking.firewall.allowedTCPPorts = [ 8001 ];
  networking.firewall.allowedUDPPorts = [ 8001 ];

  clan.core.vars.generators.hyprspace = {
    files.private-key = { };
    files.peer-id = {
      deploy = false;
      secret = false;
    };
    runtimeInputs = [
      inputs.hyprspace.packages.${pkgs.hostPlatform.system}.hyprspace
      pkgs.jq
    ];
    script = ''
      hyprspace init -c $out/hyprspace.json  | tail -n+3 | jq '.name = "${config.clan.core.settings.machine.name}"' > $out/peer-id
      jq -r '.privateKey' < $out/hyprspace.json > $out/private-key
    '';
  };

}
