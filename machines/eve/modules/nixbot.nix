{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  inherit (self.inputs.nixbot.lib) interpolate;

  codecov-upload = pkgs.writeShellScript "codecov-upload" ''
    # Only upload for test builds from harmonia (which include coverage)
    if [[ "$PROJECT" == *"harmonia"* ]] && [[ "$ATTR_NAME" == *"tests"* ]]; then
      # Find the coverage JSON file (named after system, e.g., x86_64-linux.json)
      coverage_file=$(find "$OUT_PATH" -name "*.json" -type f | head -1)
      if [[ -z "$coverage_file" ]]; then
        echo "No coverage JSON found in $OUT_PATH"
        exit 0
      fi

      # Extract system/architecture from filename (e.g., x86_64-linux.json -> x86_64-linux)
      system=$(basename "$coverage_file" .json)

      echo "Uploading coverage: slug=$PROJECT branch=$BRANCH sha=$REVISION file=$coverage_file system=$system"

      # Build codecov args (no --disable-search: we're in a git checkout so codecov can discover network files)
      set -x
      args=(
        --token "$CODECOV_TOKEN"
        --slug "$PROJECT"
        --git-service github
        --file "$coverage_file"
        --sha "$REVISION"
        --flag "$system"
      )

      if [[ -n "$PR_NUMBER" ]]; then
        args+=(--pr "$PR_NUMBER")
      else
        args+=(--branch "$BRANCH")
      fi

      # upload-process combines create-commit, create-report, and do-upload
      # It handles parallel uploads correctly (codecov merges reports automatically)
      ${pkgs.codecov-cli}/bin/codecovcli upload-process "''${args[@]}"
    else
      echo "Skipping codecov: project=$PROJECT attr=$ATTR_NAME"
    fi
  '';

  webUnixSocket = "/run/nixbot/web.sock";
in
{
  clan.core.vars.generators.buildbot-gitlab = {
    files.token = { };
    prompts.token.description = "GitLab access token (api scope)";
    script = "cp $prompts/token $out/token";
  };

  # Credentials of the "buildbot" user on git.thalheim.io
  clan.core.vars.generators.buildbot-gitea = {
    files.token = { };
    files.oauth-secret = { };
    prompts.token.description = "Gitea access token (write:repository, read:user)";
    prompts.oauth-secret.description = "Gitea OAuth client secret";
    script = ''
      cp $prompts/token $out/token
      cp $prompts/oauth-secret $out/oauth-secret
    '';
  };

  # OIDC client for Authelia: buildbot gets the plaintext secret,
  # Authelia gets the pbkdf2 digest (see authelia.nix).
  clan.core.vars.generators.buildbot-oidc = {
    files.client-secret = { };
    files.client-secret-hash.secret = false;
    runtimeInputs = with pkgs; [
      coreutils
      openssl
      authelia
      gnused
    ];
    script = ''
      openssl rand -hex 32 | tr -d '\n' > "$out/client-secret"
      authelia crypto hash generate pbkdf2 --variant sha512 \
        --password "$(cat "$out/client-secret")" |
        sed 's/^Digest: //' > "$out/client-secret-hash"
    '';
  };

  # Codecov token for harmonia coverage uploads (used in postBuildSteps)
  clan.core.vars.generators.codecov-token = {
    files.token = { };
    prompts.token.description = "Codecov upload token for harmonia";
    script = "cp $prompts/token $out/token";
  };

  # Harmonia effects secrets (JSON format for buildbot-effects)
  clan.core.vars.generators.harmonia-effects-secrets = {
    files.secrets.secret = true;
    prompts.codecov-token.description = "Codecov upload token for harmonia effects";
    script = ''
      ${pkgs.jq}/bin/jq -n \
        --arg token "$(cat $prompts/codecov-token)" \
        '{ codecov: { data: { token: $token } } }' \
        > $out/secrets
    '';
  };

  # nixbot runs as a flakelet: its units are evaluated from the nixbot flake
  # on this machine at runtime, so nixbot deploys are decoupled from the NixOS
  # generation. The host keeps the user, PostgreSQL, nginx, secrets and
  # tmpfiles below; secret paths map to systemd credential IDs referenced by
  # the raw nixbot config.
  services.flakelets = {
    enable = true;
    services.nixbot = {
      flake = "github:Mic92/nixbot";
      autoUpdate.enable = true;
      settings = {
        user = "nixbot";
        listen = webUnixSocket;
        domain = "nixbot.thalheim.io";
        credentials = {
          "github-app-secret-key" = config.sops.secrets.buildbot-github-app-secret-key.path;
          "github-webhook-secret" = config.sops.secrets.buildbot-github-webhook-secret.path;
          "github-oauth-secret" = config.sops.secrets.buildbot-github-oauth-secret.path;
          "gitea-token" = config.clan.core.vars.generators.buildbot-gitea.files.token.path;
          "gitea-oauth-secret" = config.clan.core.vars.generators.buildbot-gitea.files.oauth-secret.path;
          "gitlab-token" = config.clan.core.vars.generators.buildbot-gitlab.files.token.path;
          "oidc-client-secret" = config.clan.core.vars.generators.buildbot-oidc.files.client-secret.path;
          "codecov-token" = config.clan.core.vars.generators.codecov-token.files.token.path;
          "effects-secret__github_colon_nix-community_slash_harmonia" =
            config.clan.core.vars.generators.harmonia-effects-secrets.files.secrets.path;
          "effects-secret__github_colon_Mic92_slash_dotfiles" =
            config.clan.core.vars.generators.step-ca-renew-effect-secrets.files.secrets.path;
        };
        # Raw nixbot-config.json; secret files are credential IDs from above.
        config = {
          build_systems = [
            "i686-linux"
            "x86_64-linux"
            "aarch64-linux"
            "aarch64-darwin"
          ];
          eval_systems = [ ];
          url = "https://nixbot.thalheim.io/";
          webhook_base_url = null;
          state_dir = "/var/lib/nixbot";
          admins = [
            "github:Mic92"
            "gitea:Mic92"
            "github:DavHau"
            "github:Lassulus"
            "github:Enzime"
            "github:Kranzes"
            "oidc:auth.thalheim.io:joerg@thalheim.io"
          ];
          # Anyone who can log in through Authelia may see private repos.
          private_repo_viewers."*" = [ "oidc:auth.thalheim.io:*" ];
          eval_max_memory_size = 2048;
          eval_worker_count = 6;
          # Builds offload to tribuchet workers and use little local CPU, so
          # the default core-count cap left the workers idle. Keep this <=
          # id-count/65536 (256): each build holds one auto-allocated uid slot.
          build_concurrency = 128;
          github = {
            id = 915265;
            api_url = "https://api.github.com";
            secret_key_file = "github-app-secret-key";
            webhook_secret_file = "github-webhook-secret";
            filters = {
              user_allowlist = null;
              repo_allowlist = null;
              topic = "build-with-buildbot";
            };
            oauth_id = "Iv23ctDGhrm116Be1LhO";
            oauth_secret_file = "github-oauth-secret";
          };
          gitea = {
            instance_url = "https://git.thalheim.io";
            filters = {
              user_allowlist = null;
              repo_allowlist = null;
              topic = "build-with-buildbot";
            };
            token_file = "gitea-token";
            oauth_id = "18f7b270-a19e-4b2a-b69e-4e99f9fd7fba";
            oauth_secret_file = "gitea-oauth-secret";
            ssh_private_key_file = null;
            ssh_known_hosts_file = null;
          };
          gitlab = {
            instance_url = "https://gitlab.com";
            filters = {
              user_allowlist = null;
              repo_allowlist = [ "Mic92/dotfiles" ];
              topic = "build-with-buildbot";
            };
            token_file = "gitlab-token";
            oauth_id = null;
            oauth_secret_file = null;
            ssh_private_key_file = null;
            ssh_known_hosts_file = null;
          };
          oidc = {
            name = "Authelia";
            discovery_url = "https://auth.thalheim.io/.well-known/openid-configuration";
            client_id = "buildbot";
            scope = [
              "openid"
              "email"
              "profile"
              "groups"
            ];
            mapping = {
              username = "sub";
              groups = "groups";
            };
            client_secret_file = "oidc-client-secret";
          };
          pull_based = {
            repositories.sizelint = {
              name = "sizelint";
              default_branch = "main";
              url = "https://github.com/a-kenji/sizelint";
              poll_interval = 60;
              ssh_private_key_file = null;
              ssh_known_hosts_file = null;
            };
            poll_spread = null;
          };
          workload_identity = {
            enable = true;
            signing_key_file = null;
            token_ttl = 300;
            key_rotation_days = 30;
          };
          outputs_path = "/var/www/buildbot/nix-outputs/";
          # Upload coverage reports to codecov for harmonia
          post_build_steps = [
            {
              name = "Upload coverage to codecov";
              environment = {
                CODECOV_TOKEN = interpolate "%(secret:codecov-token)s";
                ATTR_NAME = interpolate "%(prop:attr)s";
                OUT_PATH = interpolate "%(prop:out_path)s";
                BRANCH = interpolate "%(prop:branch)s";
                REVISION = interpolate "%(prop:revision)s";
                PROJECT = interpolate "%(prop:project)s";
                PR_NUMBER = interpolate "%(prop:pr_number)s";
              };
              command = [ "${codecov-upload}" ];
              warn_only = true;
            }
          ];
          failed_build_report_limit = 47;
          # Keep buildbot-era status contexts; repos still require
          # buildbot/nix-build in branch protection.
          status_context_prefix = "buildbot";
          branches.mergeQueue = {
            matchGlob = "gitea-mq/*";
            registerGCRoots = true;
            updateOutputs = false;
          };
          gcroots_dir = "/nix/var/nix/gcroots/per-user/nixbot";
          effects_per_repo_secrets = {
            "github:nix-community/harmonia" = "effects-secret__github_colon_nix-community_slash_harmonia";
            "github:Mic92/dotfiles" = "effects-secret__github_colon_Mic92_slash_dotfiles";
          };
          effects_extra_sandbox_paths = [ ];
          effects_mountables_file = null;
          effects_extra_nix_options = { };
          show_trace_on_failure = false;
          cache_failed_builds = false;
          allow_unauthenticated_control = false;
          proxy_auth_header = null;
          build_max_silent_time = 60 * 20;
          build_timeout = 60 * 60 * 3;
          http_port = 8010;
          http_unix_socket = webUnixSocket;
          db_url = "postgresql://nixbot@/nixbot?host=/run/postgresql";
        };
      };
    };
  };

  users.users.nixbot = {
    isSystemUser = true;
    group = "nixbot";
    home = "/var/lib/nixbot";
  };
  users.groups.nixbot = { };
  # The web socket is group-restricted (0660).
  users.users.nginx.extraGroups = [ "nixbot" ];

  nix.settings.extra-allowed-users = [ "nixbot" ];

  services.postgresql.enable = true;

  # Routing and database provisioning come from the nixbot flakelet's
  # http/v1 export and postgres/v1 claim via these bridges.
  services.flakelet-nginx = {
    enable = true;
    tls = {
      certificate = "/var/lib/acme/thalheim.io/fullchain.pem";
      key = "/var/lib/acme/thalheim.io/key.pem";
    };
  };
  services.flakelet-postgres.enable = true;

  # Push deploys from nixbot CI via step-ca SSH certs. The forced command
  # only enqueues a detached update so it survives nixbot restarting itself.
  users.users.nixbot-deploy = {
    isSystemUser = true;
    group = "nixbot-deploy";
    shell = pkgs.bash;
  };
  users.groups.nixbot-deploy = { };

  environment.etc."ssh/nixbot-deploy-ca.pub".source =
    config.clan.core.vars.generators.step-ssh-user-ca.files."ca.pub".path;
  # sshd StrictModes rejects symlinks into /nix/store
  environment.etc."ssh/nixbot-deploy-principals" = {
    text = ''
      repo:github:Mic92/nixbot:ref:refs/heads/main
      repo:github:Mic92/nixbot:ref:refs/heads/flakelet
    '';
    mode = "0444";
  };

  services.openssh.extraConfig = ''
    Match User nixbot-deploy
      TrustedUserCAKeys /etc/ssh/nixbot-deploy-ca.pub
      AuthorizedPrincipalsFile /etc/ssh/nixbot-deploy-principals
      ForceCommand /run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl start --no-block flakelet-update-nixbot.service && echo "eve: flakelet update of nixbot enqueued"
  '';

  security.sudo.extraRules = [
    {
      users = [ "nixbot-deploy" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/systemctl start --no-block flakelet-update-nixbot.service";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  systemd.services.flakelet-update-nixbot = {
    description = "flakelet update of nixbot triggered by CI";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lib.getExe config.services.flakelets.package} update nixbot";
    };
  };

  # Legacy domain: permanently redirect to the new nixbot domain so old
  # links (status contexts, nix-outputs URLs, bookmarks) keep working.
  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations."/".return = "301 https://nixbot.thalheim.io$request_uri";
  };
}
