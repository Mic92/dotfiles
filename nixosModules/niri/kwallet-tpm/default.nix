# Unlock KWallet automatically at login using a password sealed in the TPM.
#
# The password is encrypted with `systemd-creds encrypt --user` and stored
# at ~/.config/kwallet-tpm/password.cred.  A systemd user service decrypts
# it and launches ksecretd via the pam_kwallet startup protocol (the only
# non-interactive unlock path left since KWallet 6.29 dropped pamOpen()).
#
# Setup (one-time, as your user):
#   echo -n 'YOUR_KWALLET_PASSWORD' | systemd-creds encrypt --user - ~/.config/kwallet-tpm/password.cred
#
# This replaces kwallet-pam, so PAM-based kwallet unlock must be disabled.
{ pkgs, ... }:
let
  ksecretd = "${pkgs.kdePackages.kwallet}/bin/ksecretd";

  kwallet-tpm-unlock = pkgs.python3.pkgs.buildPythonApplication {
    pname = "kwallet-tpm-unlock";
    version = "0.2.0";
    pyproject = false;

    nativeBuildInputs = [ pkgs.makeWrapper ];

    dontUnpack = true;

    installPhase = ''
      install -Dm755 ${./kwallet-tpm-unlock.py} $out/bin/kwallet-tpm-unlock
      wrapProgram $out/bin/kwallet-tpm-unlock \
        --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.systemd ]}
    '';
  };
in
{
  # Disable PAM-based kwallet unlock — we handle it via TPM instead
  security.pam.services.greetd.kwallet.enable = false;

  environment.systemPackages = [ kwallet-tpm-unlock ];

  systemd.user.services.kwallet-tpm-unlock = {
    description = "KWallet secret service (ksecretd) unlocked via TPM-sealed credentials";
    # ksecretd needs WAYLAND_DISPLAY etc. from the compositor for its dialogs.
    after = [ "graphical-session-pre.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    # Route D-Bus activation of ksecretd's names through this unit so an early
    # org.freedesktop.secrets client doesn't start a locked instance instead.
    aliases = [
      "dbus-org.kde.secretservicecompat.service"
      "dbus-org.freedesktop.secrets.service"
    ];
    unitConfig = {
      # A failing decrypt in a tight restart loop exhausts TPM session slots
      # (TPM_RC_CONTEXT_GAP) and breaks every other /dev/tpmrm0 user.
      StartLimitIntervalSec = 120;
      StartLimitBurst = 3;
    };
    serviceConfig = {
      Type = "dbus";
      BusName = "org.kde.secretservicecompat";
      ExecStart = "${kwallet-tpm-unlock}/bin/kwallet-tpm-unlock ${ksecretd} %h/.config/kwallet-tpm/password.cred";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  # Make the session bus hand activation to systemd (and thus the alias above)
  # instead of spawning ksecretd directly.
  services.dbus.packages = [
    (pkgs.runCommand "ksecretd-dbus-activation" { } ''
      dir=$out/share/dbus-1/services
      mkdir -p $dir
      for name in org.kde.secretservicecompat org.freedesktop.secrets; do
        cat > $dir/$name.service <<EOF
      [D-BUS Service]
      Name=$name
      Exec=${pkgs.coreutils}/bin/false
      SystemdService=kwallet-tpm-unlock.service
      EOF
      done
    '')
  ];
}
