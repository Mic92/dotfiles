# n8n Workflow Automation

n8n is configured with:

- **PostgreSQL database** for reliable data storage
- **OAuth2-proxy + Kanidm SSO** for authentication
- **Header-based auto-login** via external hooks

## URLs

- **Main UI**: https://n8n.thalheim.io/
- **Webhooks**: Use the configured webhook URL: `https://n8n.thalheim.io/`

## Authentication

Users authenticate via **Kanidm SSO** (oauth2-proxy). Once authenticated, n8n
automatically logs them in using the `X-Email` header via an external hook.

### Adding New Users

New users must be created in both Kanidm and n8n:

#### 1. Add user to Kanidm

```bash
# SSH to eve

# Login as idm_admin
kanidm login -H https://kanidm.thalheim.io -D idm_admin

# Create the user
kanidm person create <username> "<Full Name>" -H https://kanidm.thalheim.io -D idm_admin

# Set their email
kanidm person update <username> --mail <user@example.com> -H https://kanidm.thalheim.io -D idm_admin

# Create password reset token for them
kanidm person credential create-reset-token <username> -H https://kanidm.thalheim.io -D idm_admin
# Send them the reset link to set their password
```

#### 2. Add user to n8n (via n8n UI)

1. Login to n8n at https://n8n.thalheim.io/ as the owner
2. Go to **Settings** → **Users**
3. Click **Invite User**
4. Enter their email address (must match their Kanidm email)
5. Select their role (Admin, Member, etc.)
6. Click **Invite**

#### 3. User can now access n8n

Once both steps are complete:

1. User visits https://n8n.thalheim.io/
2. oauth2-proxy redirects to Kanidm for authentication
3. User logs in with their Kanidm credentials
4. Hook detects their email in the `X-Email` header
5. Hook finds their n8n account and issues JWT cookie
6. User is automatically logged into n8n

## Troubleshooting

### "User not found" error after Kanidm login

This means the user exists in Kanidm but not in n8n. Invite them via the n8n UI
(see step 2 above).

### Hook not working

The external hook file should automatically find the router module. If it fails,
check:

```bash
ssh root@eve.i "journalctl -u n8n | grep 'Problem loading external hook'"
```
