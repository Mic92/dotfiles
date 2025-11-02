{
  config,
  pkgs,
  ...
}:
let
  # Get the parent directory of the generated keys (e.g., /var/lib/sops-nix/activation/secureboot)
  # by taking the directory of one of the key files and going up one level to get to secureboot/
  securebootDir = dirOf (
    dirOf (dirOf config.clan.core.vars.generators.secureboot.files."keys/PK/PK.key".path)
  );

  # Create sbctl configuration file pointing to clan vars activation directory
  sbctlConfig = pkgs.writeText "sbctl.conf" ''
    keydir: ${securebootDir}/keys
  '';
in
{
  boot.loader.limine = {
    enable = true;
    enableEditor = true;
    maxGenerations = 20;
    # efiInstallAsRemovable = true;
    secureBoot.enable = true;
    secureBoot.configFile = sbctlConfig;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  # Clan vars generator for secureboot keys
  # Create files with subdirectory structure: keys/PK/, keys/KEK/, keys/db/
  clan.core.vars.generators.secureboot = {
    files."keys/PK/PK.key".neededFor = "activation";
    files."keys/PK/PK.pem".secret = false;
    files."keys/PK/PK.pem".neededFor = "activation";

    files."keys/KEK/KEK.key".neededFor = "activation";
    files."keys/KEK/KEK.pem".secret = false;
    files."keys/KEK/KEK.pem".neededFor = "activation";

    files."keys/db/db.key".neededFor = "activation";
    files."keys/db/db.pem".secret = false;
    files."keys/db/db.pem".neededFor = "activation";

    runtimeInputs = [ pkgs.sbctl ];
    script = ''
      # Generate secure boot keys (disable landlock since we're in a Nix sandbox)
      sbctl --disable-landlock create-keys

      # Move entire keys directory to output
      mv /var/lib/sbctl/keys "$out/keys"
    '';
  };

  # Also create symlinks in /var/lib/sbctl for runtime sbctl operations
  systemd.tmpfiles.rules = [
    "d /var/lib/sbctl 0755 root root -"
    "L+ /var/lib/sbctl/keys - - - - ${securebootDir}/keys"
  ];
}
