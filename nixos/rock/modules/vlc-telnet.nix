{ pkgs
, config
, ...
}: {
  sops.secrets.vlc-password.owner = "joerg";
  systemd.user.services.vlc = {
    wantedBy = [ "default.target" ];
    script = ''
      exec ${pkgs.vlc}/bin/vlc -I telnet --telnet-password $(cat ${config.sops.secrets.vlc-password.path})
    '';

    serviceConfig.Environment = [
      "XDG_RUNTIME_DIR=/run/user/1000"
    ];
  };

  # eve
  networking.firewall.extraCommands = ''
    ip6tables -I nixos-fw -p tcp -s 42:0:3c46:70c7:8526:2adf:7451:8bbb --dport 4212 -j nixos-fw-accept
  '';
}
