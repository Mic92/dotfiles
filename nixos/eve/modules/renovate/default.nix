{ config, pkgs, inputs, ... }:
let
  apptoken = pkgs.writeShellApplication {
    name = "apptoken";
    runtimeInputs = with pkgs; [ jq openssl curl ];
    text = builtins.readFile ./apptoken.sh;
  };
  # Based on https://docs.renovatebot.com/modules/platform/github/#running-as-a-github-app
  # https://github.com/settings/apps/mic92-renovate
  appId = "375151"; # app id of the renovate app
  appLogin = "Mic92"; # user or organization name that installed the app
in
{
  systemd.services.renovate = {
    environment = {
      RENOVATE_CONFIG_FILE = pkgs.writers.writeJSON "renovate.json" {
        labels = [ "dependencies" "renovate" ];
        nix.enabled = true;
        lockFileMaintenance.enabled = true;
        autodiscover = true;
        autodiscoverTopics = [ "managed-by-renovate" ];
        baseDir = "/var/lib/renovate/";
        cacheDir = "/var/lib/renovate/cache";
        username = "mic92-renovate[bot]";
        gitAuthor = "Mic92's Renovate Bot <142113131+mic92-renovate[bot]@users.noreply.github.com>";
      };
      LOG_LEVEL = "debug";
    };
    startAt = "weekly";
    path = [
      pkgs.git
      pkgs.openssh
      config.nix.package
      # for Cargo.lock
      pkgs.rustc
      pkgs.cargo
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      StartLimitBurst = 3;
      RuntimeDirectory = "renovate";
      StateDirectory = "renovate";
      User = "renovate";
      Group = "renovate";
      ExecStartPre = "+${pkgs.writeShellScript "setup-apptoken" ''
        set -euo pipefail
        export APP_ID=${appId}
        export APP_LOGIN=${appLogin}
        export APP_PRIVATE_KEY=$(cat ${config.sops.secrets.github-renovate-app-private-key.path})
        echo "RENOVATE_TOKEN=$(${apptoken}/bin/apptoken)" > /var/lib/renovate/env
      ''}";
    };
    script = ''
      set -exuo pipefail
      export $(cat /var/lib/renovate/env)
      rm /var/lib/renovate/env
      ${inputs.nur-packages.packages.${pkgs.hostPlatform.system}.renovate}/bin/renovate
    '';
  };
  users.users.renovate = {
    isSystemUser = true;
    createHome = false;
    home = "/var/lib/renovate";
    group = "renovate";
  };
  users.groups.renovate = { };
}
