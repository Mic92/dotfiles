{ lib, ... }: {
  services.nfs.server.enable = false;
  # nix-shell -p nfs-utils 'mount.nfs 129.215.90.50:/home /home -o nolock'
  services.nfs.server.exports = ''
    /home/joerg/web ${lib.concatMapStringsSep " " (host: ''${host}(rw,nohide,insecure,no_subtree_check,no_root_squash)'') ["192.168.32.51"]}
  '';
}
