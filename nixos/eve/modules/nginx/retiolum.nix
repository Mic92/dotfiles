{ pkgs, ... }:

{
  services.nginx.virtualHosts."retiolum.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "/var/www/retiolum.thalheim.io";
  };

  systemd.services.stockholm-autosync = {
    startAt = "hourly";
    script = ''
      set -eu -o pipefail
      readonly workdir=/var/lib/gitea/builds/stockholm-auto-update
      readonly downstream=gitea@git.thalheim.io:Mic92/stockholm
      readonly upstream=https://cgit.lassul.us/stockholm
      if [[ ! -d "$workdir" ]]; then
        git clone "$downstream" "$workdir"
      fi
      cd "$workdir"
      git fetch origin master
      git reset --hard origin/master
      git pull --rebase "$upstream" master
      git push origin +master
    '';
    path = [ pkgs.git ];
    serviceConfig = {
      Type = "oneshot";
      User = "gitea";
    };
  };
}
