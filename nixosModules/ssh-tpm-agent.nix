{ pkgs, config, ... }:
let
  askPasswordWrapper = pkgs.writeScript "ssh-askpass-wrapper" ''
    #! ${pkgs.runtimeShell} -e
    export DISPLAY="$(systemctl --user show-environment | sed 's/^DISPLAY=\(.*\)/\1/; t; d')"
    export XAUTHORITY="$(systemctl --user show-environment | sed 's/^XAUTHORITY=\(.*\)/\1/; t; d')"
    export WAYLAND_DISPLAY="$(systemctl --user show-environment | sed 's/^WAYLAND_DISPLAY=\(.*\)/\1/; t; d')"
    exec ${config.programs.ssh.askPassword} "$@"
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
    pkgs.ssh-tpm-agent
    pkgs.keyutils
  ];

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
      ExecStart = "${pkgs.ssh-tpm-agent}/bin/ssh-tpm-agent";
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
