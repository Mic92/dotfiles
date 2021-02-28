{ ... }:
{
  services.nfs.server.enable = false;
  # nix-shell -p nfs-utils 'mount.nfs 129.215.90.50:/home /home -o nolock'
  services.nfs.server.exports = ''
    /home                 129.215.90.4(rw,fsid=root,nohide,insecure,no_subtree_check)
  '';
}

