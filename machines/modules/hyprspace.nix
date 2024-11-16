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
    privateKeyFile = "/run/credentials/hyprspace.service/private-key";

    # Same as the config file
    settings.peers =
      let
        machineDirs = builtins.readDir "${config.clan.core.clanDir}/vars/per-machine";
        peers = lib.filterAttrs (
          name: type:
          type == "directory"
          && name != config.clan.core.machineName
          && builtins.pathExists "${config.clan.core.clanDir}/vars/per-machine/${name}/hyprspace/peer-id/value"

        ) machineDirs;
      in
      lib.mapAttrsToList (
        name: _:
        builtins.fromJSON (
          builtins.readFile "${config.clan.core.clanDir}/vars/per-machine/${name}/hyprspace/peer-id/value"
        )
      ) peers;
  };

  networking.firewall.allowedTCPPorts = [ 8001 ];
  networking.firewall.allowedUDPPorts = [ 8001 ];

  systemd.services.hyprspace = {
    serviceConfig.LoadCredential = [
      "private-key:${config.clan.core.vars.generators.hyprspace.files.private-key.path}"
    ];
  };

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
      hyprspace init -c $out/hyprspace.json  | tail -n+3 | jq '.name = "${config.clan.core.machineName}"' > $out/peer-id
      key=$(jq -r '.privateKey' < $out/hyprspace.json)
      echo $key > $out/private-key
    '';
  };

}
