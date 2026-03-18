# Unlock KWallet automatically at login using a password sealed in the TPM.
#
# The password is encrypted with `systemd-creds encrypt --user` and stored
# at ~/.config/kwallet-tpm/password.cred.  A systemd user service decrypts
# it at graphical-session start and feeds the derived hash to kwalletd6 via
# D-Bus pamOpen().
#
# Setup (one-time, as your user):
#   echo -n 'YOUR_KWALLET_PASSWORD' | systemd-creds encrypt --user - ~/.config/kwallet-tpm/password.cred
#
# This replaces kwallet-pam, so PAM-based kwallet unlock must be disabled.
{ pkgs, ... }:
let
  kwallet-tpm-unlock = pkgs.python3.pkgs.buildPythonApplication {
    pname = "kwallet-tpm-unlock";
    version = "0.1.0";
    pyproject = false;

    propagatedBuildInputs = [ pkgs.python3.pkgs.dbus-python ];

    # systemd-creds is needed at runtime for decryption
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
    description = "Unlock KWallet using TPM-sealed credentials";
    after = [ "dbus.socket" ];
    before = [ "graphical-session-pre.target" ];
    wantedBy = [ "graphical-session-pre.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${kwallet-tpm-unlock}/bin/kwallet-tpm-unlock %h/.config/kwallet-tpm/password.cred";
      # Retry once on failure (e.g. kwalletd6 not ready yet)
      Restart = "on-failure";
      RestartSec = 2;
      RestartMode = "direct";
    };
  };
}
