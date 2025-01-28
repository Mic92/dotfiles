{
  config,
  lib,
  pkgs,
  ...
}:
let
  secretsDir = config.clan.core.settings.directory + "/sops/secrets";
  groupsDir = config.clan.core.settings.directory + "/sops/groups";

  # My symlink is in the nixos module detected as a directory also it works in the repl. Is this because of pure evaluation?
  containsSymlink =
    path:
    builtins.pathExists path
    && (builtins.readFileType path == "directory" || builtins.readFileType path == "symlink");

  containsMachine =
    parent: name: type:
    type == "directory"
    && containsSymlink "${parent}/${name}/machines/${config.clan.core.settings.machine.name}";

  containsMachineOrGroups =
    name: type:
    (containsMachine secretsDir name type)
    || lib.any (
      group: type == "directory" && containsSymlink "${secretsDir}/${name}/groups/${group}"
    ) groups;

  filterDir =
    filter: dir:
    lib.optionalAttrs (builtins.pathExists dir) (lib.filterAttrs filter (builtins.readDir dir));

  groups = builtins.attrNames (filterDir (containsMachine groupsDir) groupsDir);
  secrets = filterDir containsMachineOrGroups secretsDir;
in
{
  config = {
    #config = lib.mkIf (config.clan.core.facts.secretStore == "sops") {
    # Before we generate a secret we cannot know the path yet, so we need to set it to an empty string
    #clan.core.facts.secretPathFunction =
    #  secret:
    #  config.sops.secrets.${"${config.clan.core.machineName}-${secret.config.name}"}.path
    #    or "/no-such-path";
    #clan.core.facts.secretModule = "clan_cli.facts.secret_modules.sops";
    #clan.core.facts.secretUploadDirectory = lib.mkDefault "/var/lib/sops-nix";

    sops.secrets = builtins.mapAttrs (name: _: {
      sopsFile = config.clan.core.settings.directory + "/sops/secrets/${name}/secret";
      format = "binary";
    }) secrets;
    # To get proper error messages about missing secrets we need a dummy secret file that is always present
    sops.defaultSopsFile = lib.mkIf config.sops.validateSopsFiles (
      lib.mkDefault (builtins.toString (pkgs.writeText "dummy.yaml" ""))
    );

    sops.age.keyFile = lib.mkIf (builtins.pathExists (
      config.clan.core.settings.directory
      + "/sops/secrets/${config.clan.core.settings.machine.name}-age.key/secret"
    )) (lib.mkDefault "/var/lib/sops-nix/key.txt");
  };
}
