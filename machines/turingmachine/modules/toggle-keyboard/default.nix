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

  # Re-enable i8042 keyboard after resume so we can login
  systemd.services.i8042-resume = {
    description = "Reload i8042 keyboard module after resume";
    after = [
      "suspend.target"
      "hibernate.target"
      "hybrid-sleep.target"
    ];
    wantedBy = [
      "suspend.target"
      "hibernate.target"
      "hybrid-sleep.target"
    ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.kmod}/bin/modprobe i8042";
    };
  };
}
