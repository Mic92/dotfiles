{ pkgs, ... }:

{
  services.nginx.virtualHosts."retiolum.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "/var/lib/gitea/builds/retiolum.thalheim.io";
  };

  systemd.timers.stockholm-autosync = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnBootSec="60min";
    timerConfig.OnUnitActiveSec="60min";
  };

  systemd.services.stockholm-autosync = {
    script = ''
      set -eux -o pipefail
      readonly workdir=/var/lib/gitea/builds/stockholm-auto-update
      readonly downstream=gitea@git.thalheim.io:Mic92/stockholm
      readonly upstream=https://cgit.krebsco.de/stockholm 
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
