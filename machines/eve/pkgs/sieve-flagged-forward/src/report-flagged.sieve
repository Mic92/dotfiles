require ["vnd.dovecot.pipe", "copy", "imapsieve", "environment", "variables", "imap4flags"];

# Only fire when \Flagged was added (not removed)
if not environment :contains "imap.changedflags" "\\Flagged" {
  stop;
}

if environment :matches "imap.user" "*" {
  set "username" "${1}";
}

if environment :matches "imap.mailbox" "*" {
  set "mailbox" "${1}";
}

pipe :copy "notify-flagged" [ "${username}", "${mailbox}" ];
