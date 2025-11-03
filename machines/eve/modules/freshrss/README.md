# FreshRSS Setup

FreshRSS is configured with Authelia SSO authentication for the web interface
and a separate API endpoint for mobile apps.

## Web Access

**URL**: https://rss.devkid.net

- Protected by Authelia SSO
- Requires LDAP authentication (group: `freshrss`)
- Use your regular credentials

## Mobile App Access

**API URL**: https://rss-api.devkid.net

- No Authelia - uses FreshRSS native API authentication
- Requires API password (set in FreshRSS Profile settings)

### Recommended Android App

**Read You** - Modern Material You RSS reader

- Available on F-Droid: https://f-droid.org/packages/me.ash.reader/
- GitHub: https://github.com/Ashinch/ReadYou

### Configuration

1. **Server**: `https://rss-api.devkid.net`
2. **API Type**: Google Reader API
3. **Username**: Your FreshRSS username (e.g., `joerg@thalheim.io`)
4. **Password**: Your API password (NOT your Authelia password)

### Setting Your API Password

**Option 1: Via Web UI**

1. Log into https://rss.devkid.net with Authelia
2. Go to **Profile** (user settings)
3. Set an **API password** in the API Management section
4. Use this password in your mobile app

**Option 2: Via CLI (on eve)**

```bash
ssh eve.r
cd /run/current-system/sw/share/FreshRSS
sudo -u freshrss env DATA_PATH=/var/lib/freshrss \
  ./cli/update-user.php --user 'your-email@domain.com' \
  --api-password 'YourSecurePassword'
```

## Architecture

- **Main domain** (`rss.devkid.net`): Web UI with Authelia SSO
- **API domain** (`rss-api.devkid.net`): API-only endpoint without Authelia
- **Database**: PostgreSQL (Unix socket)
- **Authentication**:
  - Web: `http_auth` via Authelia REMOTE_USER header
  - API: Google Reader API uses custom token-based auth (apps handle this
    automatically)
