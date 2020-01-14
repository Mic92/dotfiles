{ pkgs, ... }: {
  networking.networkmanager = {
    enable = true;
    dispatcherScripts = [{
      source = pkgs.writeText "up-hook" ''
        #!${pkgs.python3.interpreter}
        import sys
        import subprocess
        import os

        iface = os.environ.get("DEVICE_IFACE", None)
        action = os.environ.get("NM_DISPATCHER_ACTION", None)
        print(iface, file=sys.stderr)
        print(action, file=sys.stderr)

        if action in ["down", "pre-down"]:
            sys.exit(0)

        ip6 = []
        found = False
        for key, val in os.environ.items():
            if key.startswith("IP6_ADDRESS_"):
                if val.startswith("2001:630:3c1:90"):
                    found = True
                    break
        cmd = ["${pkgs.systemd}/bin/machinectl", "shell", "--uid=1000", ".host", "${pkgs.alsaUtils}/bin/amixer", "set", "Master", "mute"]
        if found:
            subprocess.run(cmd)
      '';
    }];
  };
}
