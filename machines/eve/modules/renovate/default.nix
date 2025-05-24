{
  config,
  pkgs,
  lib,
  ...
}:
let
  # Based on https://docs.renovatebot.com/modules/platform/github/#running-as-a-github-app
  # https://github.com/settings/apps/mic92-renovate
  appId = "375151"; # app id of the renovate app
  appLogin = "Mic92"; # user or organization name that installed the app
  apptoken = pkgs.writeShellApplication {
    name = "apptoken";
    runtimeInputs = with pkgs; [
      jq
      openssl
      curl
    ];
    text = builtins.readFile ./apptoken.sh;
  };
in
{
  nix.settings.allowed-users = [ "renovate" ];
  services.renovate = {
    enable = true;
    runtimePackages = [
      pkgs.git
      pkgs.openssh
      config.nix.package
      # for Cargo.lock
      pkgs.rustc
      pkgs.cargo
      # for go.mod
      pkgs.go
      pkgs.nodejs
    ];
    settings = {
      labels = [
        "dependencies"
        "renovate"
      ];
      nix.enabled = true;
      lockFileMaintenance.enabled = true;
      automerge = true;
      autodiscover = true;
      autodiscoverTopics = [ "managed-by-renovate" ];
      onboarding = true;
      username = "mic92-renovate[bot]";
      gitAuthor = "Mic92's Renovate Bot <142113131+mic92-renovate[bot]@users.noreply.github.com>";
      #allowedCommands = [
      #  "^tslint --fix$"
      #  "^tslint --[a-z]+$"
      #];
    };
    schedule = "*:0/10";

    package = pkgs.renovate.overrideAttrs (
      final: prev: {
        version = "40.0.6+sandro";

        src = pkgs.fetchFromGitHub {
          owner = "SuperSandro2000";
          repo = "renovate";
          rev = "d3c715c0285f2d1186dcb2e889e0bda96d093cb6";
          hash = "sha256-dviGWdVtBBD9PvXv5EJDy+s+wT/fcIhKYtO+mCzBD5o=";
        };

        pnpmDeps = prev.pnpmDeps.override {
          inherit (final) pname version src;
          hash = "sha256-v3coZiCgZm2eQDQTFtTdGqqUOXmjMIXuCHqJk1tdFys=";
        };
      }
    );
  };

  systemd.services.renovate = {
    serviceConfig.RuntimeDirectory = [ "renovate" ];
    serviceConfig.ExecStartPre = [
      "+${pkgs.writeShellScript "setup-apptoken" ''
        set -euo pipefail
        export APP_ID=${appId}
        export APP_LOGIN=${appLogin}
        export APP_PRIVATE_KEY=$(cat ${config.clan.core.vars.generators.renovate.files.app-private-key.path})
        echo "RENOVATE_TOKEN=$(${apptoken}/bin/apptoken)" > /run/renovate/env
        chown renovate:renovate /run/renovate/env
      ''}"
    ];
    script = lib.mkBefore ''
      export $(cat /run/renovate/env)
    '';
  };

  clan.core.vars.generators.renovate = {
    prompts.app-private-key = {
      type = "multiline";
      persist = true;
      description = "Go to https://git.clan.lol/user/settings/applications and create a new token";
    };
  };
}
