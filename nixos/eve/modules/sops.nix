{ lib, ... }: {
  # we use age instead of ssh keys
  sops.age.sshKeyPaths = lib.mkForce [];
}
