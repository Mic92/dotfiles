{
  writeShellScriptBin,
  msmtp,
  notmuch,
}:

# Wrapper for msmtp that saves sent mail to maildir
writeShellScriptBin "msmtp" ''
  # Create temp file for the email
  tmpfile=$(mktemp)
  trap "rm -f $tmpfile" EXIT

  # Read email from stdin and save to temp file
  cat > "$tmpfile"

  # Send the email with real msmtp
  if ${msmtp}/bin/msmtp "$@" < "$tmpfile"; then
      # If send successful, save to Sent folder
      # Generate unique filename for maildir
      timestamp=$(date +%s)
      hostname=$(hostname)
      pid=$$
      random=$RANDOM
      filename="''${timestamp}.''${pid}_''${random}.''${hostname}:2,S"

      # Ensure Sent directory exists
      mkdir -p "$HOME/mail/thalheim.io/.Sent/cur"

      # Save to Sent/cur directory with Seen flag
      cp "$tmpfile" "$HOME/mail/thalheim.io/.Sent/cur/$filename"

      # Update notmuch database
      ${notmuch}/bin/notmuch new >/dev/null 2>&1

      exit 0
  else
      # If send failed, exit with msmtp's exit code
      exit $?
  fi
''
