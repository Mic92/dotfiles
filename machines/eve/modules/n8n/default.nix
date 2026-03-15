{
  config,
  pkgs,
  self,
  ...
}:
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
  # Allow n8n to write to OpenCrow's trigger pipe.
  systemd.services.n8n.serviceConfig.SupplementaryGroups = [ "opencrow" ];

  clan.core.vars.generators.n8n-task-runner = {
    files.auth-token.secret = true;
    script = ''
      printf '%s' "$(dd if=/dev/urandom bs=32 count=1 2>/dev/null | base64)" > "$out/auth-token"
    '';
  };

  services.n8n.taskRunners.enable = true;

  services.n8n = {
    enable = true;
    environment = {
      N8N_RUNNERS_AUTH_TOKEN_FILE =
        config.clan.core.vars.generators.n8n-task-runner.files.auth-token.path;
      WEBHOOK_URL = "https://n8n.thalheim.io/";

      # Database configuration
      DB_TYPE = "postgresdb";
      DB_POSTGRESDB_HOST = "/run/postgresql";
      DB_POSTGRESDB_DATABASE = "n8n";
      DB_POSTGRESDB_USER = "n8n";

      # Executions pruning
      EXECUTIONS_DATA_PRUNE = "true";
      EXECUTIONS_DATA_MAX_AGE = "336"; # 2 weeks

      # Custom hooks and SSO configuration
      EXTERNAL_HOOK_FILES = "${hooksFile}";
      N8N_FORWARD_AUTH_HEADER = "X-Email";
      N8N_SSO_HOSTNAME = "n8n.thalheim.io";
    };
  };

  # Install community nodes into custom directory
  # n8n automatically loads *.node.js and *.credentials.js from ~/.n8n/custom
  systemd.services.n8n.preStart =
    let
      micsNodes = self.inputs.mics-n8n-nodes.packages.${pkgs.stdenv.hostPlatform.system};
      linkCommands = pkgs.lib.concatStringsSep "\n" (
        pkgs.lib.mapAttrsToList (
          name: pkg: "ln -sfn ${pkg}/lib/node_modules/${name}/dist /var/lib/n8n/.n8n/custom/${name}"
        ) micsNodes
      );
      n8n-nodes-paperless = self.packages.${pkgs.stdenv.hostPlatform.system}.n8n-nodes-paperless;
    in
    ''
      mkdir -p /var/lib/n8n/.n8n/custom
      ln -sfn ${
        self.inputs.n8n-nodes-caldav.packages.${pkgs.stdenv.hostPlatform.system}.default
      }/lib/node_modules/n8n-nodes-caldav/dist \
        /var/lib/n8n/.n8n/custom/n8n-nodes-caldav
      ln -sfn ${n8n-nodes-paperless}/lib/node_modules/@n8n-chezmoi-sh/n8n-nodes-paperless/dist \
        /var/lib/n8n/.n8n/custom/n8n-nodes-paperless
      ${linkCommands}
    '';

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

    # Exclude webhooks and OAuth callback from Authelia authentication
    # Webhooks need to be publicly accessible for external services
    # OAuth credential endpoints need direct access for Google OAuth flow,
    # as the auth_request to Authelia fails with 431 due to oversized headers
    locations."~ ^/(webhook|webhook-test|rest/webhook|rest/oauth2-credential)" = {
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

    # MCP Server Trigger endpoints with SSE support
    locations."~ ^/mcp/" = {
      proxyPass = "http://127.0.0.1:5678";
      extraConfig = ''
        # Prevent header injection - explicitly clear auth headers
        proxy_set_header X-Email "";
        proxy_set_header X-User "";
        proxy_set_header X-Auth-Request-User "";
        proxy_set_header X-Auth-Request-Email "";
        proxy_set_header X-Access-Token "";

        # n8n MCP requires this Accept header for both GET and POST requests
        proxy_set_header Accept "application/json, text/event-stream";

        # SSE (Server-Sent Events) configuration for MCP endpoints
        proxy_http_version 1.1;
        proxy_buffering off;
        proxy_set_header Connection "";
        chunked_transfer_encoding off;

        # Increase timeouts for long-lived SSE connections
        proxy_read_timeout 86400s;
        proxy_send_timeout 86400s;
      '';
    };

    # All other endpoints
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
