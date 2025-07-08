{
  config,
  lib,
  ...
}:
let
  cfg = config.services.borgbackup-zfs-snapshots;
  zfs = config.boot.zfs.package;

  # Get all ZFS filesystems
  zfsFileSystems = lib.filter (fs: fs.fsType == "zfs") (lib.attrValues config.fileSystems);

  # Map paths to their ZFS datasets (find the most specific match)
  pathToDataset =
    path:
    let
      matching = lib.filter (fs: lib.hasPrefix fs.mountPoint path) zfsFileSystems;
      # Sort by length descending to get the most specific match first
      sorted = lib.sort (a: b: lib.stringLength a.mountPoint > lib.stringLength b.mountPoint) matching;
    in
    if sorted != [ ] then lib.head sorted else null;

  # Get all backup paths from clan.core.state
  allBackupPaths = lib.unique (
    lib.flatten (map (state: state.folders or [ ]) (lib.attrValues config.clan.core.state))
  );

  # Get all unique ZFS datasets used for backups
  allBackupDatasets = lib.unique (lib.filter (d: d != null) (map pathToDataset allBackupPaths));

  # Find root datasets (datasets that are not children of other datasets in our list)
  rootDatasets = lib.filter (
    ds: !lib.any (other: ds != other && lib.hasPrefix "${other.device}/" ds.device) allBackupDatasets
  ) allBackupDatasets;

  # Check if we have any ZFS datasets to backup
  hasZfsBackups = allBackupDatasets != [ ];

  # Transform a backup path to use the snapshot directory
  transformPathToSnapshot =
    name: path:
    let
      dataset = pathToDataset path;
    in
    if dataset != null then
      # Replace the mount point with the .zfs/snapshot path
      let
        relativePath = lib.removePrefix dataset.mountPoint path;
        # Ensure we have a leading slash for non-empty relative paths
        relativePathWithSlash =
          if relativePath == "" then
            ""
          else if lib.hasPrefix "/" relativePath then
            relativePath
          else
            "/${relativePath}";
      in
      "${dataset.mountPoint}/.zfs/snapshot/borg-${name}${relativePathWithSlash}"
    else
      path;
in
{
  options.services.borgbackup-zfs-snapshots = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = hasZfsBackups;
      description = "Enable ZFS snapshot support for borgbackup jobs";
    };
  };

  # Extend the borgbackup jobs submodule type
  options.services.borgbackup.jobs = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { config, name, ... }:
        {
          options = {
            useZfsSnapshots = lib.mkOption {
              type = lib.types.bool;
              default = cfg.enable && hasZfsBackups;
              description = "Use ZFS snapshots for this backup job";
            };

            # Override the paths option to add apply function
            paths = lib.mkOption {
              apply = paths: if config.useZfsSnapshots then map (transformPathToSnapshot name) paths else paths;
            };
          };

          # Add hooks for snapshot management
          config = lib.mkIf config.useZfsSnapshots {
            # Add pre-hook to create recursive snapshots
            preHook = lib.mkBefore ''
              echo "Creating ZFS snapshots for borgbackup job ${name}"
              set -e

              # Create recursive snapshots for root datasets only
              ${lib.concatMapStringsSep "\n" (fs: ''
                if ${zfs}/bin/zfs list -H -o name "${fs.device}" >/dev/null 2>&1; then
                  ${zfs}/bin/zfs snapshot -r "${fs.device}@borg-${name}" || {
                    echo "Failed to create recursive snapshot for ${fs.device}"
                    exit 1
                  }
                  echo "Created recursive snapshot: ${fs.device}@borg-${name}"
                fi
              '') rootDatasets}

              # Ensure snapshot directories are accessible (trigger automount)
              echo "Ensuring snapshot directories are accessible..."
              ${lib.concatMapStringsSep "\n" (fs: ''
                ls "${fs.mountPoint}/.zfs/snapshot/borg-${name}/" > /dev/null || {
                  echo "Warning: Could not access snapshot directory ${fs.mountPoint}/.zfs/snapshot/borg-${name}/"
                }
              '') allBackupDatasets}

              set +e
            '';

            # Add post-hook to destroy recursive snapshots
            postHook = lib.mkAfter ''
              echo "Cleaning up ZFS snapshots for borgbackup job ${name}"

              # Destroy recursive snapshots (only need to destroy root datasets)
              ${lib.concatMapStringsSep "\n" (fs: ''
                if ${zfs}/bin/zfs list -H -o name "${fs.device}@borg-${name}" >/dev/null 2>&1; then
                  ${zfs}/bin/zfs destroy -r "${fs.device}@borg-${name}" || echo "Warning: Failed to destroy recursive snapshot ${fs.device}@borg-${name}"
                fi
              '') rootDatasets}
            '';
          };
        }
      )
    );
  };

  config = lib.mkIf cfg.enable {
    # Extend systemd services for borgbackup jobs that use ZFS snapshots
    systemd.services = lib.mapAttrs' (
      name: job:
      lib.nameValuePair "borgbackup-job-${name}" (
        lib.mkIf (job.useZfsSnapshots or false) {
          serviceConfig = {
            PrivateDevices = lib.mkForce false; # ZFS needs access to /dev/zfs
          };

          path = [ zfs ];
        }
      )
    ) config.services.borgbackup.jobs;
  };
}
