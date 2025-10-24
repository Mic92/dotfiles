# Kanidm Identity Management

Kanidm provides identity management and OAuth2/OIDC SSO for various services on
eve.

## URLs

- **Web UI**: https://kanidm.thalheim.io/
- **LDAP**: `localhost:3636` (internal only)
- **OAuth2/OIDC**: `https://kanidm.thalheim.io/oauth2/openid/<client-name>`

## Admin Accounts

- **admin**: System administration account
- **idm_admin**: Identity management account (create users, groups, OAuth2
  clients)

Passwords are stored in rbw:

- `rbw get kanidm-admin`
- `rbw get kanidm-idm_admin`

## Creating New Users

```bash
# SSH to eve
ssh root@eve.i

# Login as idm_admin
kanidm login -H https://kanidm.thalheim.io -D idm_admin

# Create person account
kanidm person create <username> "<Full Legal Name>" -H https://kanidm.thalheim.io -D idm_admin

# Set email address
kanidm person update <username> --mail <email@example.com> -H https://kanidm.thalheim.io -D idm_admin

# Create password reset token
kanidm person credential create-reset-token <username> -H https://kanidm.thalheim.io -D idm_admin
# This will output a reset URL like: https://kanidm.thalheim.io/ui/reset?token=xxxxx
# Send this link to the user to set their password

# Add user to groups (example: n8n_users)
kanidm group add-members n8n_users <username> -H https://kanidm.thalheim.io -D idm_admin
```

## Creating New OAuth2 Applications

### Option 1: Use Shared oauth2-proxy (Recommended)

For most web applications, use the shared oauth2-proxy instance:

1. **Create a group for the application**:

```bash
ssh root@eve.i "kanidm group create <appname>_users -H https://kanidm.thalheim.io -D idm_admin"
```

2. **Add users to the group**:

```bash
ssh root@eve.i "kanidm group add-members <appname>_users <username> -H https://kanidm.thalheim.io -D idm_admin"
```

3. **Add to NixOS configuration** (`machines/eve/modules/<appname>.nix`):

```nix
{
  services.oauth2-proxy.nginx.virtualHosts."<appname>.thalheim.io" = {
    allowed_groups = [ "<appname>_users@kanidm.thalheim.io" ];
  };

  services.nginx.virtualHosts."<appname>.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations."/" = {
      proxyPass = "http://127.0.0.1:<port>";
    };
  };
}
```

The shared oauth2-proxy will handle authentication automatically.

### Option 2: Dedicated OAuth2 Client

For applications with native OAuth2/OIDC support:

```bash
# SSH to eve and login
ssh root@eve.i
kanidm login -H https://kanidm.thalheim.io -D idm_admin

# Create OAuth2 client
kanidm system oauth2 create <client-name> '<Display Name>' https://<app>.thalheim.io -H https://kanidm.thalheim.io -D idm_admin

# Add redirect URL (adjust based on app's callback URL)
kanidm system oauth2 add-redirect-url <client-name> 'https://<app>.thalheim.io/auth/callback' -H https://kanidm.thalheim.io -D idm_admin

# Configure scopes (grant access to specific group)
kanidm system oauth2 update-scope-map <client-name> <group-name> openid email profile groups -H https://kanidm.thalheim.io -D idm_admin

# Disable PKCE if app doesn't support it
kanidm system oauth2 warning-insecure-client-disable-pkce <client-name> -H https://kanidm.thalheim.io -D idm_admin

# Set landing URL
kanidm system oauth2 set-landing-url <client-name> https://<app>.thalheim.io/ -H https://kanidm.thalheim.io -D idm_admin

# Get client secret
kanidm system oauth2 show-basic-secret <client-name> -H https://kanidm.thalheim.io -D idm_admin
```

#### Configure in application:

- **Client ID**: `<client-name>`
- **Client Secret**: (from show-basic-secret command)
- **OIDC Discovery URL**:
  `https://kanidm.thalheim.io/oauth2/openid/<client-name>/.well-known/openid-configuration`
- **Issuer URL**: `https://kanidm.thalheim.io/oauth2/openid/<client-name>`
- **Scopes**: `openid email profile` (add `groups` if needed)

## Current OAuth2 Clients

- **oauth2-proxy**: Shared authentication proxy for multiple applications
  - Used by: n8n
  - Groups: Application-specific (e.g., `n8n_users`)

## Recovering Admin Accounts

If you need to reset admin passwords:

```bash
ssh root@eve.i

# For system admin
kanidmd recover-account admin -c /nix/store/*-server.toml

# For identity admin
kanidmd recover-account idm_admin -c /nix/store/*-server.toml
```

This will output a temporary password. Store it in rbw and change it via the web
UI.

## LDAP

Kanidm provides LDAP on `localhost:3636`. Configure applications with:

- **Host**: `localhost` or `127.0.0.1`
- **Port**: `389` (if using local OpenLDAP proxy) or `3636` (direct Kanidm LDAP)
- **Base DN**: Depends on your LDAP configuration

## Useful Commands

```bash
# List all OAuth2 clients
kanidm system oauth2 list -H https://kanidm.thalheim.io -D idm_admin

# Get details of a specific client
kanidm system oauth2 get <client-name> -H https://kanidm.thalheim.io -D idm_admin

# List all groups
kanidm group list -H https://kanidm.thalheim.io -D idm_admin

# Show group members
kanidm group list-members <group-name> -H https://kanidm.thalheim.io -D idm_admin

# Show user details
kanidm person get <username> -H https://kanidm.thalheim.io -D idm_admin
```

## Backups

Kanidm performs automatic online backups daily at midnight UTC to
`/var/lib/kanidm/backup/`.
