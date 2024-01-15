{ pkgs, ... }:
{
  security.wrappers.toggle-keyboard = {
    source = "${pkgs.callPackage ./toggle-keyboard.nix {}}/bin/toggle-keyboard";
    setuid = true;
    owner = "root";
    group = "root";
  };
  powerManagement.enable = true;
  # re-enable after suspend so we can login
  powerManagement.powerUpCommands = "${pkgs.kmod}/bin/modprobe i8042";
}
