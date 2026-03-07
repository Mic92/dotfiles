{
  pkgs,
  ...
}:
{
  services.opencrow.skills = [
    {
      name = "email";
      path = ./skills/email;
    }
  ];
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

  services.opencrow.extraPackages = [ pkgs.mblaze ];
}
