{ pkgs, ... }: {
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
      rm -rf "$workdir"
      git clone "$downstream" "$workdir"
      cd "$workdir"
      git pull --rebase "$upstream" master
      git push --force origin master
    '';
    path = [
      pkgs.git
      pkgs.openssh
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "gitea";
    };
  };
}
