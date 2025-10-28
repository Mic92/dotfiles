{ config, pkgs, ... }:
{
  # Shared oauth2-proxy configuration for multiple apps
  # This provides SSO via Kanidm for any app that needs authentication

  # Vars generator for oauth2-proxy secrets
  clan.core.vars.generators.oauth2-proxy = {
    files.env-file = {
      secret = true;
    };

    prompts.client-secret = {
      description = "Kanidm OAuth2 client secret for oauth2-proxy";
      type = "hidden";
      persist = true;
      display = {
        group = "oauth2-proxy";
        label = "Kanidm OAuth2 Client Secret";
        helperText = ''
          Get this from: ssh root@eve.i "kanidm system oauth2 show-basic-secret oauth2-proxy -H https://kanidm.thalheim.io -D idm_admin"
        '';
      };
    };

    runtimeInputs = with pkgs; [
      coreutils
      openssl
    ];

    script = ''
            CLIENT_SECRET=$(cat "$prompts/client-secret" | tr -d '\n')
            # Generate 32 bytes for cookie secret, encoded as URL-safe base64 (no padding)
            COOKIE_SECRET=$(openssl rand 32 | openssl base64 -A | tr '+/' '-_' | tr -d '=')

            cat > "$out/env-file" <<EOF
      OAUTH2_PROXY_CLIENT_SECRET=$CLIENT_SECRET
      OAUTH2_PROXY_COOKIE_SECRET=$COOKIE_SECRET
      EOF
    '';
  };

  # Shared oauth2-proxy service
  services.oauth2-proxy = {
    enable = true;
    package = pkgs.oauth2-proxy;

    provider = "oidc";

    clientID = "oauth2-proxy";

    # Use keyFile for sensitive configuration
    keyFile = config.clan.core.vars.generators.oauth2-proxy.files.env-file.path;

    # Email configuration - allow all authenticated users
    email.domains = [ "*" ];

    # Reverse proxy settings
    reverseProxy = true;
    httpAddress = "127.0.0.1:4180";

    # OIDC configuration
    oidcIssuerUrl = "https://kanidm.thalheim.io/oauth2/openid/oauth2-proxy";
    redirectURL = "https://thalheim.io/oauth2/callback";

    # Cookie settings
    cookie = {
      domain = ".thalheim.io";
      secure = true;
      httpOnly = true;
      name = "_oauth2_proxy";
    };

    # Pass authentication headers
    setXauthrequest = true;

    # Additional configuration
    extraConfig = {
      skip-provider-button = true;
      whitelist-domain = ".thalheim.io";
      oidc-groups-claim = "groups";
    };

    # Nginx integration - domain for OAuth2 endpoints
    nginx = {
      domain = "thalheim.io";
      proxy = "http://127.0.0.1:4180";
      # virtualHosts will be added by individual app modules
    };
  };
}
