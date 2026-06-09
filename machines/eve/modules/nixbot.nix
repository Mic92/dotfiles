{
  config,
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

  services.nixbot = {
    enable = true;
    domain = "buildbot.thalheim.io";
    # Keep buildbot-era status contexts; repos still require
    # buildbot/nix-build in branch protection.
    statusContextPrefix = "buildbot";

    buildSystems = [
      "i686-linux"
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    branches.mergeQueue.matchGlob = "gitea-mq/*";
    evalWorkerCount = 6;
    github = {
      enable = true;
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;

      oauthId = "Iv23ctDGhrm116Be1LhO";
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;

      appId = 915265;
      appSecretKeyFile = config.sops.secrets.buildbot-github-app-secret-key.path;
    };
    gitea = {
      enable = true;
      instanceUrl = "https://git.thalheim.io";
      tokenFile = config.clan.core.vars.generators.buildbot-gitea.files.token.path;
      oauthId = "18f7b270-a19e-4b2a-b69e-4e99f9fd7fba";
      oauthSecretFile = config.clan.core.vars.generators.buildbot-gitea.files.oauth-secret.path;
    };
    gitlab = {
      enable = true;
      tokenFile = config.clan.core.vars.generators.buildbot-gitlab.files.token.path;
      repoAllowlist = [ "Mic92/dotfiles" ];
    };
    oidc = {
      enable = true;
      name = "Authelia";
      discoveryUrl = "https://auth.thalheim.io/.well-known/openid-configuration";
      clientId = "buildbot";
      clientSecretFile = config.clan.core.vars.generators.buildbot-oidc.files.client-secret.path;
      scope = [
        "openid"
        "email"
        "profile"
        "groups"
      ];
      mapping.groups = "groups";
    };
    # Anyone who can log in through Authelia may see private repos.
    privateRepoViewers = {
      "*" = [ "oidc:auth.thalheim.io:*" ];
    };
    admins = [
      "github:Mic92"
      "gitea:Mic92"
      "github:DavHau"
      "github:Lassulus"
      "github:Enzime"
      "oidc:auth.thalheim.io:joerg@thalheim.io"
    ];
    outputsPath = "/var/www/buildbot/nix-outputs/";

    # Upload coverage reports to codecov for harmonia
    postBuildSteps = [
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
        warnOnly = true;
      }
    ];

    # Secrets for buildbot-effects (hercules-ci style effects)
    effects.perRepoSecretFiles = {
      "github:nix-community/harmonia" =
        config.clan.core.vars.generators.harmonia-effects-secrets.files.secrets.path;
    };

    pullBased = {
      repositories = {
        sizelint = {
          url = "https://github.com/a-kenji/sizelint";
          defaultBranch = "main";
        };
      };
    };
  };

  systemd.services.nixbot.serviceConfig.LoadCredential = [
    "codecov-token:${config.clan.core.vars.generators.codecov-token.files.token.path}"
  ];

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
  };
}
