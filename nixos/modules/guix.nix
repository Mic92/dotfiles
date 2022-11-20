{ config, lib, pkgs, ... }:

let
  buildUser = i: lib.nameValuePair "guixbuilder${i}" {
    group = "guixbuild";
    extraGroups = ["guixbuild"];
    home = "/var/empty";
    description = "Guix build user ${i}";
    isSystemUser = true;
  };
in
{
  users.users = lib.listToAttrs
    (builtins.map (i: buildUser (lib.fixedWidthNumber 2 i))
      (lib.range 1 10));
  users.extraGroups.guixbuild = {
    name = "guixbuild";
  };

  systemd.services.guix-daemon = {
    wantedBy = [ "multi-user.target" ];
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild --discover=no";
      Environment = "GUIX_LOCPATH=/home/joerg/.guix-profile/lib/locale";
      # Despite the name, this is rate-limited: a broken daemon will eventually fail.
      Restart = "always";
      StandardOutput = "syslog";
      StandardError = "syslog";
      # See <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00608.html>.
      # Some package builds (for example, go@1.8.1) may require even more than
      # 1024 tasks.
      TasksMax = "8192";
      # Work around a nasty systemd ‘feature’ that kills the entire process tree
      # (including the daemon!) if any child, such as cc1plus, runs out of memory.
      OOMPolicy = "continue";
    };
  };
}
