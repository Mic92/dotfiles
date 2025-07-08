{
  config,
  lib,
  pkgs,
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
      matching = lib.filter (fs: lib.hasPrefix fs.mountpoint path) zfsFileSystems;
      # Sort by length descending to get the most specific match first
      sorted = lib.sort (a: b: lib.stringLength a.mountpoint > lib.stringLength b.mountpoint) matching;
    in
    if sorted != [ ] then lib.head sorted else null;

  # Get all backup paths from clan.core.state
  allBackupPaths = lib.unique (
    lib.flatten (map (state: state.folders or [ ]) (lib.attrValues config.clan.core.state))
  );

  # Get all unique ZFS datasets used for backups
  allBackupDatasets = lib.unique (lib.filter (d: d != null) (map pathToDataset allBackupPaths));

  # Create mount commands for snapshots (mount parents before children)
  mountSnapshotCommands =
    datasets:
    let
      # Sort by mountpoint length ascending (shortest first)
      sortedDatasets = lib.sort (
        a: b: lib.stringLength a.mountpoint < lib.stringLength b.mountpoint
      ) datasets;
    in
    lib.concatMapStringsSep "\n" (fs: ''
      # Mount ${fs.device} snapshot
      if ${zfs}/bin/zfs list -H -o name "${fs.device}@borg" >/dev/null 2>&1; then
        mkdir -p "${cfg.mountRoot}${fs.mountpoint}"
        mount -t zfs "${fs.device}@borg" "${cfg.mountRoot}${fs.mountpoint}"
        echo "Mounted ${fs.device}@borg at ${cfg.mountRoot}${fs.mountpoint}"
      fi
    '') sortedDatasets;

  # Unmount all snapshots recursively
  umountSnapshotCommands = ''
    # Recursively unmount all snapshots under the backup root
    if mountpoint -q "${cfg.mountRoot}"; then
      umount -R "${cfg.mountRoot}" || echo "Warning: Failed to recursively unmount ${cfg.mountRoot}"
    fi
  '';
in
{
  options.services.borgbackup-zfs-snapshots = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.services.borgbackup.jobs != { } && allBackupDatasets != [ ];
      description = "Enable ZFS snapshot support for borgbackup";
    };

    mountRoot = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/backup-root";
      description = "Root directory where ZFS snapshots will be mounted for backup";
    };
  };

  config = lib.mkIf cfg.enable {
    # Ensure the backup root directory exists
    systemd.tmpfiles.rules = [
      "d ${cfg.mountRoot} 0755 root root -"
    ];

    # Override borgbackup job paths to use the snapshot mount root
    services.borgbackup.jobs = lib.mapAttrs (
      name: job:
      let
        # Check if any of the clan.core.state paths are on ZFS
        hasZfs = allBackupDatasets != [ ];
      in
      if hasZfs then
        job
        // {
          paths = lib.mkForce (map (path: "${cfg.mountRoot}${path}") allBackupPaths);

          preHook = ''
            ${job.preHook or ""}

            # Create and mount ZFS snapshots
            set -e
            echo "Creating ZFS snapshots for borgbackup job ${name}"

            # Create snapshots
            ${lib.concatMapStringsSep "\n" (fs: ''
              if ${zfs}/bin/zfs list -H -o name "${fs.device}" >/dev/null 2>&1; then
                ${zfs}/bin/zfs snapshot "${fs.device}@borg" || {
                  echo "Failed to create snapshot for ${fs.device}"
                  exit 1
                }
                echo "Created snapshot: ${fs.device}@borg"
              fi
            '') allBackupDatasets}

            # Mount snapshots under backup root
            ${mountSnapshotCommands allBackupDatasets}

            # Verify all paths are accessible
            ${lib.concatMapStringsSep "\n" (path: ''
              if ! ls "${cfg.mountRoot}${path}" >/dev/null 2>&1; then
                echo "Error: Backup path ${cfg.mountRoot}${path} is not accessible"
                exit 1
              fi
            '') allBackupPaths}

            set +e
          '';

          postHook = ''
            exitStatus=$?

            # Unmount and destroy snapshots
            echo "Cleaning up ZFS snapshots for borgbackup job ${name}"

            # Unmount snapshots
            ${umountSnapshotCommands}

            # Destroy snapshots
            ${lib.concatMapStringsSep "\n" (fs: ''
              if ${zfs}/bin/zfs list -H -o name "${fs.device}@borg" >/dev/null 2>&1; then
                ${zfs}/bin/zfs destroy "${fs.device}@borg" || echo "Warning: Failed to destroy snapshot ${fs.device}@borg"
              fi
            '') allBackupDatasets}

            ${job.postHook or ""}
          '';
        }
      else
        job
    ) config.services.borgbackup.jobs;

    # Ensure borgbackup services have required permissions
    systemd.services = lib.mapAttrs' (
      name: _job:
      lib.nameValuePair "borgbackup-job-${name}" (
        lib.mkIf (allBackupDatasets != [ ]) {
          path = [
            zfs
            pkgs.util-linux
          ];
          serviceConfig = {
            PrivateDevices = lib.mkForce false; # ZFS needs access to /dev/zfs
            # Need CAP_SYS_ADMIN for mounting
            AmbientCapabilities = [ "CAP_SYS_ADMIN" ];
            CapabilityBoundingSet = [ "CAP_SYS_ADMIN" ];
            # Ensure we can write to mount root
            ReadWritePaths = [ cfg.mountRoot ];
          };
        }
      )
    ) config.services.borgbackup.jobs;
  };
}
