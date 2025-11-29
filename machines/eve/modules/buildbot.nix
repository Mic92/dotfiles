{
  config,
  pkgs,
  self,
  ...
}:
let
  inherit (self.inputs.buildbot-nix.lib) interpolate;

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

      # Extract PR number if this is a PR branch (refs/pull/NNN/merge)
      if [[ "$BRANCH" =~ refs/pull/([0-9]+)/ ]]; then
        args+=(--pr "''${BASH_REMATCH[1]}")
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
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";

    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    buildSystems = [
      "i686-linux"
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    branches.releaseBranches.matchGlob = "test-*";
    evalWorkerCount = 6;
    github = {
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;

      oauthId = "Iv23ctDGhrm116Be1LhO";
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;

      appId = 915265;
      appSecretKeyFile = config.sops.secrets.buildbot-github-app-secret-key.path;
    };
    admins = [
      "Mic92"
      "DavHau"
      "Lassulus"
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
  services.buildbot-master = {
    extraConfig = ''
      from buildbot.manhole import AuthorizedKeysManhole
      c['manhole'] = AuthorizedKeysManhole("tcp:12456", "/etc/ssh/authorized_keys.d/joerg", "/var/lib/buildbot/master/ssh/")
    '';
    pythonPackages = ps: [
      ps.bcrypt
      ps.cryptography
    ];
  };
  systemd.services.buildbot-master.path = [ pkgs.openssh ];
  systemd.services.buildbot-master.serviceConfig.LoadCredential = [
    "codecov-token:${config.clan.core.vars.generators.codecov-token.files.token.path}"
  ];
  systemd.services.buildbot-master.preStart = ''
    mkdir -p /var/lib/buildbot/master/ssh
    if [ ! -f /var/lib/buildbot/master/ssh/ssh_host_ed25519_key ]; then
      ssh-keygen -t ed25519 -f /var/lib/buildbot/master/ssh/ssh_host_ed25519_key -N ""
    fi
  '';
  services.buildbot-nix.worker = {
    enable = true;
    workerPasswordFile = config.sops.secrets.buildbot-nix-worker-password.path;
    # Broken atm
    #nixEvalJobs.package =
    #  (pkgs.nix-eval-jobs.override {
    #    nix = config.nix.package;
    #  }).overrideAttrs
    #    (_oldAttrs: {
    #      src = pkgs.fetchFromGitHub {
    #        owner = "nix-community";
    #        repo = "nix-eval-jobs";
    #        # https://github.com/nix-community/nix-eval-jobs/pull/341
    #        rev = "331aa136a3414f41e7524c9614d29a35122d6275";
    #        sha256 = "sha256-+ga7K6xenkpyoJgsM7ZYjacTLIoaCVxt33SzfTQrZpE=";
    #      };
    #    });

  };

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
  };
}
