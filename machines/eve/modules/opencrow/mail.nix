{
  config,
  self,
  pkgs,
  ...
}:
let
  n8nHooksConfig = builtins.toJSON {
    token_command = "rbw get n8n-hooks-token";
    hooks = {
      store-draft.url = "https://n8n.thalheim.io/webhook/store-email-draft";
      github.url = "https://n8n.thalheim.io/webhook/github-api";
      gitea.url = "https://n8n.thalheim.io/webhook/gitea-api";
      slack.url = "https://n8n.thalheim.io/webhook/slack-context";
    };
  };
in
{
  services.opencrow.skills.email = ./skills/email;
  # Expose starred/flagged emails (delivered by sieve-flagged-forward)
  # to Janet read-only so she can read full message bodies.
  containers.opencrow.bindMounts."/var/mail/flagged" = {
    hostPath = "/var/vmail/thalheim.io/janet/Maildir";
    isReadOnly = true;
  };

  # Ensure the Maildir exists before the container starts.
  # setgid (2) so new files inherit the opencrow group, allowing
  # Janet to read emails delivered by the vmail user.
  systemd.tmpfiles.rules = [
    "d /var/vmail/thalheim.io/janet 2770 vmail opencrow -"
    "d /var/vmail/thalheim.io/janet/Maildir 2770 vmail opencrow -"
    "d /var/vmail/thalheim.io/janet/Maildir/new 2770 vmail opencrow -"
    "d /var/vmail/thalheim.io/janet/Maildir/cur 2770 vmail opencrow -"
    "d /var/vmail/thalheim.io/janet/Maildir/tmp 2770 vmail opencrow -"
  ];

  # --- n8n-hooks: store email drafts in IMAP ---

  clan.core.vars.generators.opencrow-n8n-hooks = {
    files.n8n-hooks-token.secret = true;

    prompts.n8n-hooks-token.description = "Shared bearer token for n8n webhooks";

    script = ''
      cp "$prompts/n8n-hooks-token" "$out/n8n-hooks-token"
    '';
  };

  services.opencrow.rbwEntries."n8n-hooks-token" = "n8n-hooks-token";

  services.opencrow.credentialFiles."n8n-hooks-token" =
    config.clan.core.vars.generators.opencrow-n8n-hooks.files.n8n-hooks-token.path;

  services.opencrow.extraPackages = [
    pkgs.mblaze
    self.packages.${pkgs.stdenv.hostPlatform.system}.n8n-hooks
  ];

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/n8n-hooks 0750 opencrow opencrow -"
    "f+ /var/lib/opencrow/.config/n8n-hooks/config.json 0640 opencrow opencrow - ${n8nHooksConfig}"
  ];
}
