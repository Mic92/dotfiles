{ lib, pkgs, ... }:
let
  irc-announce = pkgs.callPackage ./irc-announce {};
  untilport = pkgs.callPackage ./untilport {};
in {
  networking.firewall.enable = false;

  networking.nameservers = [
    # hurricane electric
    "74.82.42.42" "2001:470:20::2"
  ];

  services.tor = {
    enable = true;
    hiddenServices."ssh".map = [ { port = 22; } ];
    extraConfig = ''
      SocksPort 0
      HiddenServiceNonAnonymousMode 1
      HiddenServiceSingleHopMode 1
      ExitNodes {de}
      NewCircuitPeriod 120
    '';
  };

  systemd.services.hidden-ssh-announce = {
    description = "irc announce hidden ssh";
    after = [ "tor.service" "network-online.target" ];
    wants = [ "tor.service" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      set -efu
      until test -e /var/lib/tor/onion/ssh/hostname; do
      echo "still waiting for /var/lib/tor/onion/ssh/hostname"
      sleep 1
      done
      ${untilport}/bin/untilport irc.freenode.org 6667 && \
      ${irc-announce}/bin/irc-announce \
        irc.freenode.org 6667 install-image "#krebs-announce" \
        "SSH Hidden Service at $(cat /var/lib/tor/onion/ssh/hostname)"
    '';
    serviceConfig = {
      PrivateTmp = "true";
      User = "tor";
      Type = "oneshot";
    };
  };

  services.openssh.enable = true;
  boot.zfs.enableUnstable = true;
  systemd.services.sshd.wantedBy = lib.mkForce [ "multi-user.target" ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
  ];
}
