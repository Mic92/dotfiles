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
                        // This allows n8n-api.thalheim.io to bypass oauth2-proxy safely
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

  # Use shared oauth2-proxy for authentication
  services.oauth2-proxy.nginx.virtualHosts."n8n.thalheim.io" = {
    allowed_groups = [ "n8n_users" ];
  };

  services.nginx.virtualHosts."n8n.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";

    # Exclude webhooks from oauth2-proxy authentication
    # Webhooks need to be publicly accessible for external services
    locations."~ ^/(webhook|webhook-test|rest/webhook)".extraConfig = ''
      auth_request off;
    '';

    # NOTE: API endpoints (/api/, /rest/) are NOT excluded
    # API requests go through oauth2-proxy but use n8n's API key authentication
    # The hook will not auto-login API requests (no X-Auth-Request-User header)

    locations."/" = {
      proxyPass = "http://127.0.0.1:5678";
      proxyWebsockets = true;
    };
  };

  # Separate API-only domain without oauth2-proxy
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
