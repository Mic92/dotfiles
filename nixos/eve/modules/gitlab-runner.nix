{ pkgs, config, ... }: {
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
      config.nix.package
    ];
  };

  systemd.services.gitlab-runner = {
    serviceConfig = {
      User = "gitlab-runner";
      Group = "gitlab-runner";
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
