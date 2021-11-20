{ pkgs, config, lib, ... }: {
  sops.secrets.gitlab-runner-registration.owner = "gitlab-runner";

  services.gitlab-runner = {
    enable = true;
    concurrent = 16;
    services.shell = {
      executor = "shell";
      registrationConfigFile = config.sops.secrets.gitlab-runner-registration.path;
    };
    extraPackages = with pkgs; [
      # Required stuff
      bash
      nettools # hostname
      git
      gnutar
      gzip
      rsync
      nix-eval-jobs
      config.nix.package
    ];
  };

  systemd.services.gitlab-runner = {
    confinement.enable = true;
    confinement.packages = config.services.gitlab-runner.extraPackages;
    serviceConfig = {
      User = "gitlab-runner";
      Group = "gitlab-runner";
      DynamicUser = lib.mkForce false;
      Environment = [
        "NIX_REMOTE=daemon"
        "PAGER=cat"
      ];
      BindPaths = [
        "/nix/var/nix/daemon-socket/socket"
        "/run/nscd/socket"
        "/var/lib/drone"
      ];
      BindReadOnlyPaths = [
        "/etc/passwd:/etc/passwd"
        "/etc/group:/etc/group"
        "/nix/var/nix/profiles/system/etc/nix:/etc/nix"
        "${config.environment.etc."ssl/certs/ca-certificates.crt".source}:/etc/ssl/certs/ca-certificates.crt"
        "${config.environment.etc."ssh/ssh_known_hosts".source}:/etc/ssh/ssh_known_hosts"
        "${builtins.toFile "ssh_config" ''
          Host eve.thalheim.io
            ForwardAgent yes
        ''}:/etc/ssh/ssh_config"
        "/etc/machine-id"
        # channels are dynamic paths in the nix store, therefore we need to bind mount the whole thing
        "/nix/"
      ];
    };
  };

  users.users.gitlab-runner = {
    group = "gitlab-runner";
    isSystemUser = true;
    home = "/var/lib/gitlab-runner";
  };

  users.groups.gitlab-runner = {};

  nix.allowedUsers = [ "gitlab-runner" ];
}
