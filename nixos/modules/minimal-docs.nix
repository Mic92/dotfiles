{ lib, ... }: {
  documentation.nixos.enable = lib.mkForce false;
  documentation.info.enable = false;
  documentation.doc.enable = false;
}
