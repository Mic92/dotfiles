{ pkgs, ... }:
let
  # Hook file for header-based authentication
  hooksFile = pkgs.writeText "n8n-hooks.js" ''
    const { resolve } = require('path');
    const fs = require('fs');

    // Find the router module dynamically by scanning node_modules/.pnpm
    const n8nBasePath = '${pkgs.n8n}/lib/n8n';
    const pnpmDir = resolve(n8nBasePath, 'node_modules/.pnpm');
    const routerDir = fs.readdirSync(pnpmDir).find(dir => dir.startsWith('router@'));

    const Layer = require(resolve(pnpmDir, routerDir, 'node_modules/router/lib/layer'));
    const { issueCookie } = require(resolve(n8nBasePath, 'packages/cli/dist/auth/jwt'));

    const ignoreAuthRegexp = /^\/(assets|healthz|webhook|rest\/oauth2-credential)/
    module.exports = {
        n8n: {
            ready: [
                async function ({ app }, config) {
                    const { stack } = app.router
                    const index = stack.findIndex((l) => l.name === 'cookieParser')
                    stack.splice(index + 1, 0, new Layer('/', {
                        strict: false,
                        end: false
                    }, async (req, res, next) => {
                        if (ignoreAuthRegexp.test(req.url)) return next()
                        if (!config.get('userManagement.isInstanceOwnerSetUp', false)) return next()
                        if (req.cookies?.['n8n-auth']) return next()
                        if (!process.env.N8N_FORWARD_AUTH_HEADER) return next()

                        // SECURITY: Only enable header-based auth for specific hostname
                        // This allows n8n-api.thalheim.io to bypass authelia safely
                        const allowedHost = process.env.N8N_SSO_HOSTNAME || 'n8n.thalheim.io';
                        if (req.headers.host !== allowedHost) return next()

                        const email = req.headers[process.env.N8N_FORWARD_AUTH_HEADER.toLowerCase()]
                        if (!email) return next()
                        const user = await this.dbCollections.User.findOneBy({email})
                        if (!user) {
                            res.statusCode = 401
                            res.end(`User ''${email} not found, please have an admin invite the user first.`)
                            return
                        }
                        if (!user.role) {
                            user.role = {}
                        }
                        issueCookie(res, user)
                        return next()
                    }))
                },
            ],
        },
    }
  '';
in
{
  services.n8n = {
    enable = true;
    webhookUrl = "https://n8n.thalheim.io/";
    settings = {
      database = {
        type = "postgresdb";
        postgresdb = {
          host = "/run/postgresql";
          database = "n8n";
          user = "n8n";
        };
      };
      executions.pruneData = true;
      executions.pruneDataMaxAge = 336; # 2 weeks
    };
  };

  systemd.services.n8n.environment = {
    EXTERNAL_HOOK_FILES = "${hooksFile}";
    N8N_FORWARD_AUTH_HEADER = "X-Email";
    N8N_SSO_HOSTNAME = "n8n.thalheim.io";
  };

  services.postgresql.ensureDatabases = [ "n8n" ];
  services.postgresql.ensureUsers = [
    {
      name = "n8n";
      ensureDBOwnership = true;
    }
  ];

  services.nginx.virtualHosts."n8n.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";

    # Authelia auth_request endpoint
    locations."/authelia" = {
      proxyPass = "http://127.0.0.1:9091/api/verify";
      extraConfig = ''
        internal;
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-Host $http_host;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header Content-Length "";
        proxy_pass_request_body off;
      '';
    };

    # Redirect to Authelia login on 401
    locations."@authelia_proxy_signin" = {
      extraConfig = ''
        return 302 https://auth.thalheim.io/?rd=$scheme://$http_host$request_uri;
      '';
    };

    # Exclude webhooks from authentication
    # Webhooks need to be publicly accessible for external services
    locations."~ ^/(webhook|webhook-test|rest/webhook)" = {
      proxyPass = "http://127.0.0.1:5678";
      proxyWebsockets = true;
    };

    # NOTE: API endpoints (/api/, /rest/) are NOT excluded
    # API requests go through authelia but use n8n's API key authentication
    # The hook will not auto-login API requests (no Remote-User header)

    locations."/" = {
      proxyPass = "http://127.0.0.1:5678";
      proxyWebsockets = true;
      extraConfig = ''
        # Forward auth request to Authelia
        auth_request /authelia;
        auth_request_set $user $upstream_http_remote_user;
        auth_request_set $email $upstream_http_remote_email;
        auth_request_set $name $upstream_http_remote_name;
        auth_request_set $groups $upstream_http_remote_groups;

        # Pass user info to backend
        proxy_set_header X-Email $email;
        proxy_set_header Remote-User $user;
        proxy_set_header Remote-Email $email;
        proxy_set_header Remote-Name $name;
        proxy_set_header Remote-Groups $groups;

        # Redirect to login if not authenticated
        error_page 401 = @authelia_proxy_signin;
      '';
    };
  };

  # Separate API-only domain without authelia
  # Hook will NOT auto-login requests from this domain
  services.nginx.virtualHosts."n8n-api.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";

    locations."/" = {
      proxyPass = "http://127.0.0.1:5678";
      proxyWebsockets = true;
      extraConfig = ''
        # Prevent header injection - explicitly clear auth headers
        proxy_set_header X-Email "";
        proxy_set_header X-User "";
        proxy_set_header X-Auth-Request-User "";
        proxy_set_header X-Auth-Request-Email "";
        proxy_set_header X-Access-Token "";
      '';
    };
  };
}
