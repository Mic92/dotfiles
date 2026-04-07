{
  pkgs,
  lib,
  self,
  ...
}:
let
  # Track upstream master until the next tagged release ships
  # https://github.com/Foxboron/ssh-tpm-agent/pull/114 (ssh-tpm-add -c
  # confirm-before-use + SSH_ASKPASS_PROMPT leak fix).
  # keyring tests require Linux kernel keyring which isn't available in the Nix sandbox
  ssh-tpm-agent = pkgs.ssh-tpm-agent.overrideAttrs (_old: {
    version = "0.8.0-unstable-2026-03-29";
    src = pkgs.fetchFromGitHub {
      owner = "Foxboron";
      repo = "ssh-tpm-agent";
      rev = "a3cde87934ea75ebe721d20c4d6d4da9cec0a7e4";
      hash = "sha256-Fx77S7RRXwp65xaQ1huFaAD4/bnpwXki74KuOJ+9BE0=";
    };
    # nixpkgs carries a backport patch that is already in this tree
    patches = [ ];
    vendorHash = "sha256-s899KmXdeUt/SSCL3Vu1T/JTJT8mZP99MDb+Thcfyw4=";
    doCheck = false;
  });
  # Prefer the noctalia ssh-askpass plugin when the shell is running: it
  # actually honours SSH_ASKPASS_PROMPT=confirm (lxqt-openssh-askpass does
  # not) so ssh-tpm-add -c confirmations get real Allow/Deny buttons instead
  # of an ambiguous password box. Falls back to lxqt-openssh-askpass when
  # the plugin socket is absent (shell not running, headless TTY, etc).
  noctaliaAskpass =
    lib.getExe
      self.inputs.noctalia-plugins.packages.${pkgs.stdenv.hostPlatform.system}.noctalia-ssh-askpass;
  askPasswordWrapper = pkgs.writeScript "ssh-askpass-wrapper" ''
    #! ${pkgs.runtimeShell} -e
    if [ -S "''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/noctalia-ssh-askpass.sock" ]; then
      exec ${noctaliaAskpass} "$@"
    fi
    export DISPLAY="$(systemctl --user show-environment | sed 's/^DISPLAY=\(.*\)/\1/; t; d')"
    export XAUTHORITY="$(systemctl --user show-environment | sed 's/^XAUTHORITY=\(.*\)/\1/; t; d')"
    export WAYLAND_DISPLAY="$(systemctl --user show-environment | sed 's/^WAYLAND_DISPLAY=\(.*\)/\1/; t; d')"
    exec ${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass "$@"
  '';
  # Example SSH config for using the TPM2 agent + FIDO keys for selected hosts:
  #Host *.hosts-with-yubikeys.tld
  #   IdentityFile ~/.ssh/id_ed25519_sk
  #   IdentityAgent /run/user/1000/ssh-fido-agent.sock
  #Host *
  #  IdentitiesOnly yes
  #  IdentityFile ~/.ssh/id_ecdsa_tpm.pub
  #  IdentityAgent /run/user/1000/ssh-tpm-agent.sock
in
{
  security.tpm2.enable = true;
  security.tpm2.tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  users.users.joerg.extraGroups = [ "tss" ]; # tss group has access to TPM devices

  environment.systemPackages = [
    ssh-tpm-agent
    pkgs.keyutils
  ];

  # Expose the wrapper so the niri module can point programs.ssh.askPassword
  # at it. KDE machines keep ksshaskpass (set by the plasma6 module).
  _module.args.sshAskPasswordWrapper = askPasswordWrapper;

  systemd.user.services.ssh-tpm-agent = {
    description = "ssh-tpm-agent service";
    documentation = [
      "man:ssh-agent(1)"
      "man:ssh-add(1)"
      "man:ssh(1)"
    ];
    unitConfig = {
      ConditionEnvironment = "!SSH_AGENT_PID";
      Requires = "ssh-tpm-agent.socket";
    };
    environment = {
      SSH_ASKPASS = askPasswordWrapper;
    };
    serviceConfig = {
      ExecStart = "${ssh-tpm-agent}/bin/ssh-tpm-agent";
      PassEnvironment = "SSH_AGENT_PID";
      SuccessExitStatus = 2;
      Type = "simple";
    };
  };

  systemd.user.sockets.ssh-tpm-agent = {
    wantedBy = [ "sockets.target" ];
    unitConfig = {
      Description = "SSH TPM agent socket";
      Documentation = [
        "man:ssh-agent(1)"
        "man:ssh-add(1)"
        "man:ssh(1)"
      ];
    };
    socketConfig = {
      ListenStream = "%t/ssh-tpm-agent.sock";
      SocketMode = "0600";
      Service = "ssh-tpm-agent.service";
    };
  };

  # For fido keys, we have an on-demand ssh-agent that is started

  systemd.user.services.ssh-fido-agent = {
    path = [ pkgs.gnused ];
    description = "SSH Agent for FIDO keys";
    wantedBy = [ "default.target" ];
    unitConfig = {
      ConditionUser = "!@system";
    };
    environment = {
      SSH_ASKPASS = askPasswordWrapper;
    };
    serviceConfig = {
      ExecStartPre = "${pkgs.coreutils}/bin/rm -f %t/ssh-fido-agent.sock";
      ExecStart = "${pkgs.openssh}/bin/ssh-agent -D -a %t/ssh-fido-agent.sock";
      Restart = "on-failure";
      RestartSec = "1s";
      Type = "simple";
    };
  };
}
