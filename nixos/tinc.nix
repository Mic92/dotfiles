{ config, pkgs, ... }:

let
  netname = "retiolum";
in {

  services.tinc.networks.${netname} = {
    name = "turingmachine";
    extraConfig = ''
      ConnectTo = gum
      ConnectTo = ni
      ConnectTo = prism
    '';
  };

  environment.systemPackages = [ config.services.tinc.networks.${netname}.package ];

  systemd.services."tinc.${netname}" = {
    path = with pkgs; [ curl gnutar bzip2 ];
    postStart = ''
      curl https://lassul.us/retiolum-hosts.tar.bz2 | tar -xjvf - -C /etc/tinc/${netname}/ || true
    '';
  };

  systemd.network.networks = {
    "${netname}".extraConfig = ''
      [Match]
      Name = tinc.${netname}

      [Network]
      Address=10.243.29.168/12
      Address=42:4992:6a6d:600::1/16
    '';
  };
}
