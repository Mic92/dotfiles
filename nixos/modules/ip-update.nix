{ pkgs
, config
, ...
}: {
  systemd.timers.ip-update = {
    description = "Update ip address";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnUnitActiveSec = "5min";
      OnBootSec = "5min";
    };
  };
  systemd.services.ip-update = {
    path = [ pkgs.bind.dnsutils pkgs.curl ];
    script = ''
      set -x
      ip=$(curl -4 https://ip.thalheim.io)
      ip6=$(curl -6 https://ip.thalheim.io)
      if [[ -z "$ip" && -z "$ip6" ]]; then
        exit 0
      fi
      (
        echo "server ns1.thalheim.io"
        echo "zone ${config.networking.hostName}.thalheim.io."
        echo "update delete ${config.networking.hostName}.thalheim.io. A"
        if [[ -n "$ip" ]]; then
          echo "update add ${config.networking.hostName}.thalheim.io. 600 A $ip"
        fi
        echo "update delete ${config.networking.hostName}.thalheim.io. AAAA"
        if [[ -n "$ip6" ]]; then
          echo "update add ${config.networking.hostName}.thalheim.io. 600 AAAA $ip6"
        fi
        echo "send"
      ) | nsupdate -k ${config.sops.secrets."${config.clanCore.machineName}-ip-update-key".path} -v
    '';
    serviceConfig.Type = "oneshot";
    serviceConfig.Restart = "on-failure";
  };
}
