# SSH keys and git identity for the opencrow container.
# Used by gitea (n8n-workflows repo) and any other git/ssh operations.
{
  config,
  pkgs,
  ...
}:
{
  clan.core.vars.generators.opencrow-ssh = {
    files.ssh-private-key.secret = true;
    files.ssh-public-key.secret = false;

    runtimeInputs = with pkgs; [ openssh ];

    script = ''
      ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "opencrow@eve"
      ssh-keygen -y -f "$out/ssh-private-key" > "$out/ssh-public-key"
    '';
  };

  clan.core.vars.generators.opencrow-gitea = {
    files.gitea-password.secret = true;

    runtimeInputs = with pkgs; [ openssl ];

    script = ''
      openssl rand -base64 32 | tr -d '\n' > "$out/gitea-password"
    '';
  };

  # Pass SSH keys as credentials — the opencrow service installs them
  # into ~/.ssh on startup (SSH requires user-owned key files).
  services.opencrow.credentialFiles = {
    "ssh-private-key" = config.clan.core.vars.generators.opencrow-ssh.files.ssh-private-key.path;
    "ssh-public-key" = config.clan.core.vars.generators.opencrow-ssh.files.ssh-public-key.path;
  };

  containers.opencrow.config.systemd.services.opencrow.serviceConfig.ExecStartPre = [
    "+${pkgs.writeShellScript "install-ssh-keys" ''
      install -d -m 0700 -o opencrow -g opencrow /var/lib/opencrow/.ssh
      install -m 0600 -o opencrow -g opencrow "''${CREDENTIALS_DIRECTORY}/ssh-private-key" /var/lib/opencrow/.ssh/id_ed25519
      install -m 0644 -o opencrow -g opencrow "''${CREDENTIALS_DIRECTORY}/ssh-public-key" /var/lib/opencrow/.ssh/id_ed25519.pub
    ''}"
  ];

  containers.opencrow.config.systemd.tmpfiles.rules = [
    ''f /var/lib/opencrow/.gitconfig 0644 opencrow opencrow - [user]\n\tname = Janet\n\temail = janet@thalheim.io''
  ];
}
