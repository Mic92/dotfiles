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
  security.tpm2.pkcs11.enable = true; # expose /run/current-system/sw/lib/libtpm2_pkcs11.so
  security.tpm2.tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  users.users.joerg.extraGroups = [ "tss" ]; # tss group has access to TPM devices

  environment.systemPackages = [ pkgs.ssh-tpm-agent ];

  systemd.user.services.ssh-tpm-agent = {
    path = [ pkgs.gnused ];
    description = "SSH Agent";
    wantedBy = [ "default.target" ];
    unitConfig = {
      ConditionUser = "!@system";
      ConditionEnvironment = "!SSH_AGENT_PID";
    };
    environment = {
      SSH_ASKPASS = askPasswordWrapper;
      SSH_AUTH_SOCK = "%t/ssh-tpm-agent.sock";
    };
    serviceConfig = {
      ExecStartPre = "${pkgs.coreutils}/bin/rm -f %t/ssh-agent";
      ExecStart = "${pkgs.ssh-tpm-agent}/bin/ssh-tpm-agent";
      PassEnvironment = "SSH_AGENT_PID";
      SuccessExitStatus = 2;
      Type = "simple";
    };
  };

  systemd.user.sockets.ssh-tpm-agent = {
    wantedBy = [ "sockets.target" ];
    unitConfig.Description = "SSH Agent Socket";
    socketConfig = {
      ListenStream = "%t/ssh-tpm-agent.sock";
      SocketMode = "0600";
      Service = "ssh-tpm-agent.service";
    };
  };

  # For fido keys, we have an on-demand ssh-agent that is started

  systemd.user.services.ssh-fido-agent = {
    path = [ pkgs.gnused ];
    description = "SSH Agent";
    wantedBy = [ "default.target" ];
    unitConfig = {
      ConditionUser = "!@system";
      ConditionEnvironment = "!SSH_AGENT_PID";
    };
    environment = {
      SSH_ASKPASS = askPasswordWrapper;
      SSH_AUTH_SOCK = "%t/ssh-fido-agent.sock";
    };
    serviceConfig = {
      ExecStartPre = "${pkgs.coreutils}/bin/rm -f %t/ssh-agent";
      ExecStart = "${pkgs.openssh}/bin/ssh-agent";
      PassEnvironment = "SSH_AGENT_PID";
      SuccessExitStatus = 2;
      Type = "simple";
    };
  };

  systemd.user.sockets.ssh-fido-agent = {
    wantedBy = [ "sockets.target" ];
    unitConfig.Description = "SSH Agent Socket";
    socketConfig = {
      ListenStream = "%t/ssh-fido-agent.sock";
      SocketMode = "0600";
      Service = "ssh-fido-agent.service";
    };
  };
}
