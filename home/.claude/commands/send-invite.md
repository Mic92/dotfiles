Send an email or calendar invite using msmtp (automatically saves to Sent
folder).

## Email Sending

Use Python to construct and send emails via msmtp:

```python
import subprocess
import time
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase

msg = MIMEMultipart('mixed')
msg['From'] = 'joerg@thalheim.io'
msg['To'] = '<recipient>'
msg['Subject'] = '<subject>'
msg['Date'] = time.strftime('%a, %d %b %Y %H:%M:%S %z')

body = MIMEText("<body text>", 'plain')
msg.attach(body)

# Send via msmtp (auto-saves to Sent folder)
process = subprocess.Popen(['msmtp', '-t'], stdin=subprocess.PIPE)
process.communicate(msg.as_bytes())
```

## Calendar Invites

**IMPORTANT:** Always use UTC when creating events to avoid timezone
interpretation issues. Convert the desired local time to UTC first, then create
the event with `TZ=UTC`.

1. Create the event with khal using UTC:
   ```bash
   # Convert local time to UTC first, then:
   TZ=UTC khal new -a Personal <date> <utc_start> <utc_end> "<title>" -l "<location>"
   ```

2. Find the ICS file:
   ```bash
   khal list <date> --format "{start-time}-{end-time} {title} [{uid}]"
   find ~/.local/share/calendars -name "*<UID>*"
   ```

3. Attach ICS to email:
   ```python
   with open('<ics_path>', 'r') as f:
       ics_content = f.read()

   calendar_part = MIMEBase('text', 'calendar', method='REQUEST', name='invite.ics')
   calendar_part.set_payload(ics_content)
   calendar_part.add_header('Content-Disposition', 'attachment', filename='invite.ics')
   msg.attach(calendar_part)
   ```

## Workflow

1. Ask for: recipient, subject, body, and if calendar invite:
   date/time/timezone/location
2. For calendar invites, create the khal event first (using UTC)
3. Construct the email with Python
4. Send via msmtp

User request: $ARGUMENTS
