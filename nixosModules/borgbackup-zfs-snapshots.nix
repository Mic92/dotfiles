{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.borgbackup-zfs-snapshots;
  zfs = config.boot.zfs.package;

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

  allBackupPaths = lib.unique (
    lib.flatten (map (state: state.folders or [ ]) (lib.attrValues config.clan.core.state))
  );

  allBackupDatasets = lib.unique (lib.filter (d: d != null) (map pathToDataset allBackupPaths));

  # Mount parents before children, unmount in reverse order
  datasetsByDepth = lib.sort (
    a: b: lib.stringLength a.mountPoint < lib.stringLength b.mountPoint
  ) allBackupDatasets;

  # Find root datasets (datasets that are not children of other datasets in our list)
  rootDatasets = lib.filter (
    ds: !lib.any (other: ds != other && lib.hasPrefix "${other.device}/" ds.device) allBackupDatasets
  ) allBackupDatasets;

  hasZfsBackups = allBackupDatasets != [ ];

  # Snapshots are mounted explicitly under /run/borgbackup/<job> instead of
  # using the .zfs/snapshot automount, whose device/inode can change
  # mid-backup and make borg skip the tree with "file type or inode changed".
  snapshotMountPoint =
    name: dataset:
    "/run/borgbackup/${name}" + (if dataset.mountPoint == "/" then "" else dataset.mountPoint);

  # Snapshots are mounted in a flat staging dir and bind-mounted into the
  # tree: mount.zfs refuses non-empty mountpoints, bind mounts don't.
  stagingMountPoint =
    name: dataset:
    "/run/borgbackup/.staging-${name}/"
    + (
      if dataset.mountPoint == "/" then "root" else lib.replaceStrings [ "/" ] [ "-" ] dataset.mountPoint
    );

  transformPathToSnapshot =
    name: path:
    let
      dataset = pathToDataset path;
    in
    if dataset != null then
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
      "${snapshotMountPoint name dataset}${relativePathWithSlash}"
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

            paths = lib.mkOption {
              apply = paths: if config.useZfsSnapshots then map (transformPathToSnapshot name) paths else paths;
            };
          };

          config = lib.mkIf config.useZfsSnapshots {
            preHook = lib.mkBefore ''
              set -e

              # clean up leftovers from a previous failed run
              ${lib.concatMapStringsSep "\n" (fs: ''
                ${pkgs.util-linux}/bin/umount "${snapshotMountPoint name fs}" 2>/dev/null || true
                ${pkgs.util-linux}/bin/umount "${stagingMountPoint name fs}" 2>/dev/null || true
              '') (lib.reverseList datasetsByDepth)}
              ${lib.concatMapStringsSep "\n" (fs: ''
                ${zfs}/bin/zfs destroy -r "${fs.device}@borg-${name}" 2>/dev/null || true
              '') rootDatasets}

              ${lib.concatMapStringsSep "\n" (fs: ''
                if ${zfs}/bin/zfs list -H -o name "${fs.device}" >/dev/null 2>&1; then
                  ${zfs}/bin/zfs snapshot -r "${fs.device}@borg-${name}"
                fi
              '') rootDatasets}

              ${lib.concatMapStringsSep "\n" (fs: ''
                mkdir -p "${stagingMountPoint name fs}"
                ${pkgs.util-linux}/bin/mount -t zfs -o ro "${fs.device}@borg-${name}" "${stagingMountPoint name fs}"
                mkdir -p "${snapshotMountPoint name fs}" 2>/dev/null || true
                ${pkgs.util-linux}/bin/mount --bind "${stagingMountPoint name fs}" "${snapshotMountPoint name fs}"
              '') datasetsByDepth}

              set +e
            '';

            postHook = lib.mkAfter ''
              ${lib.concatMapStringsSep "\n" (fs: ''
                ${pkgs.util-linux}/bin/umount "${snapshotMountPoint name fs}" || true
                ${pkgs.util-linux}/bin/umount "${stagingMountPoint name fs}" || true
              '') (lib.reverseList datasetsByDepth)}

              # only root datasets carry the recursive snapshot
              ${lib.concatMapStringsSep "\n" (fs: ''
                ${zfs}/bin/zfs destroy -r "${fs.device}@borg-${name}" 2>/dev/null || true
              '') rootDatasets}
            '';
          };
        }
      )
    );
  };

  config = lib.mkIf cfg.enable {
    systemd.services = lib.mapAttrs' (
      name: job:
      lib.nameValuePair "borgbackup-job-${name}" (
        lib.mkIf (job.useZfsSnapshots or false) {
          serviceConfig = {
            PrivateDevices = lib.mkForce false; # ZFS needs access to /dev/zfs
            # writable mountpoints for the ZFS snapshots (rest of /run is read-only)
            RuntimeDirectory = [
              "borgbackup/${name}"
              "borgbackup/.staging-${name}"
            ];
          };

          path = [ zfs ];
        }
      )
    ) config.services.borgbackup.jobs;
  };
}
