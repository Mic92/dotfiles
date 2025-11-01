{
  self,
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    self.inputs.hyprspace.nixosModules.default
  ];

  options = {
    services.hyprspace = {
      blockRfc1918Addresses = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          If true, blocks RFC1918 addresses using the firewall to stop hyprspace from connecting to it.
          Some providers such as Hetzner will sent out abuse reports if you connect to these addresses.
        '';
      };
    };
  };

  config = {
    systemd.services.hyprspace.serviceConfig.IPAddressDeny =
      lib.mkIf config.services.hyprspace.blockRfc1918Addresses
        [
          "10.0.0.0/8"
          "172.16.0.0/12"
          "192.168.0.0/16"
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
        self.inputs.hyprspace.packages.${pkgs.stdenv.hostPlatform.system}.hyprspace
        pkgs.jq
      ];
      script = ''
        hyprspace init -c $out/hyprspace.json  | tail -n+3 | jq '.name = "${config.clan.core.settings.machine.name}"' > $out/peer-id
        jq -r '.privateKey' < $out/hyprspace.json > $out/private-key
      '';
    };

  };

}
