{ pkgs, ... }:
{
  security.wrappers.toggle-keyboard = {
    source = "${pkgs.callPackage ./toggle-keyboard.nix { }}/bin/toggle-keyboard";
    setuid = true;
    owner = "root";
    group = "root";
  };
  environment.systemPackages = [
    #(pkgs.writeShellScriptBin "toggle-keyboard" ''
    #  #!${pkgs.stdenv.shell}
    #  if [ -e /sys/module/i8042 ]; then
    #    /run/wrapper/bin/toggle-keyboard
    #  fi

    #'')
  ];
  powerManagement.enable = true;
  # re-enable after suspend so we can login
  powerManagement.powerUpCommands = "${pkgs.kmod}/bin/modprobe i8042";
}
