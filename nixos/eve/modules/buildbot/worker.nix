{
  config,
  lib,
  pkgs,
  ...
}: let
  package = pkgs.python3Packages.buildbot-worker;
  python = package.pythonModule;
  home = "/var/lib/buildbot-worker";
  buildbotDir = "${home}/worker";
in {
  nix.settings.allowed-users = ["buildbot-worker"];
  users.users.buildbot-worker = {
    description = "Buildbot Worker User.";
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/buildbot-worker";
    group = "buildbot-worker";
    useDefaultShell = true;
  };
  users.groups.buildbot-worker = {};

  systemd.services.buildbot-worker = {
    reloadIfChanged = true;
    description = "Buildbot Worker.";
    after = ["network.target" "buildbot-master.service"];
    wantedBy = ["multi-user.target"];
    path = [pkgs.git pkgs.nix pkgs.cachix pkgs.gh];
    environment.PYTHONPATH = "${python.withPackages (p: [package])}/${python.sitePackages}";
    environment.MASTER_URL = ''tcp:host=localhost:port=9989'';
    environment.BUILDBOT_DIR = buildbotDir;
    environment.WORKER_PASSWORD_FILE = config.sops.secrets.buildbot-nix-worker-password.path;

    serviceConfig = {
      Type = "simple";
      User = "buildbot-worker";
      Group = "buildbot-worker";
      WorkingDirectory = home;

      # Restart buildbot with a delay. This time way we can use buildbot to deploy itself.
      ExecReload = "+${pkgs.systemd}/bin/systemd-run --on-active=60 ${pkgs.systemd}/bin/systemctl restart buildbot-worker";
      ExecStart = "${python.pkgs.twisted}/bin/twistd --nodaemon --pidfile= --logfile - --python ${./worker.py}";
    };
  };
  sops.secrets.buildbot-nix-worker-password.owner = "buildbot-worker";
}
