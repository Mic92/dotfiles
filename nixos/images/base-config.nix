{ lib, pkgs, ... }:
let
  nur = (builtins.getFlake (toString ../..)).inputs.nur;
in
{
  networking.firewall.enable = false;

  nixpkgs.overlays = [ nur.overlay ];

  services.resolved.enable = false;
  networking.nameservers = [
    # hurricane electric
    "74.82.42.42"
    "2001:470:20::2"
  ];

  networking.usePredictableInterfaceNames = false;
  systemd.network.enable = true;
  systemd.network.networks = lib.mapAttrs'
    (num: _:
      lib.nameValuePair "eth${num}" {
        extraConfig = ''
          [Match]
          Name = eth${num}

          [Network]
          DHCP = both
          LLMNR = true
          IPv4LL = true
          LLDP = true
          IPv6AcceptRA = true
          IPv6Token = ::521a:c5ff:fefe:65d9
          # used to have a stable address for zfs send
          Address = fd42:4492:6a6d:43:1::${num}/64

          [DHCP]
          UseHostname = false
          RouteMetric = 512
        '';
      })
    { "0" = { }; "1" = { }; "2" = { }; "3" = { }; };

  imports = [
    ../modules/tor-ssh.nix
  ];

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
      ${pkgs.nur.repos.mic92.untilport}/bin/untilport irc.freenode.org 6667 && \
      ${pkgs.nur.repos.mic92.irc-announce}/bin/irc-announce \
        irc.freenode.org 6667 install-image "#krebs-announce" \
        "SSH Hidden Service at $(cat /var/lib/tor/onion/ssh/hostname)"
    '';
    serviceConfig = {
      PrivateTmp = "true";
      User = "tor";
      Type = "oneshot";
    };
  };

  systemd.services.sshd.wantedBy = lib.mkForce [ "multi-user.target" ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
    # for dropbear
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDGoQXNL3B1+pS4WYfhvn4ULb6oCNovT+dpWist7osToj5UVQ64odlcemnSG07GRcEnwf2zDTYq8eatomGQ94VsnmWuKaYzF8nqNl+qHRM49nS+Myi2ETn0B5fnMSh45lmkjR5rL/tb02EXUVoNf7acE2K3Q8M/tGFEdCdQNuqEgishi5nrs/WvZHn0cxP1anv8WRtm2qlj0jtH1rYmo7n/xsPb15FNBaE92aQXTkGoj6xdQknGWnGjLLm33lGIxRKvHTJ9T2NGte4gTYC/CADPxU2x5nq8zGDTNna/YMUyKmlqgGm+p+sE9dERmxKtquLgyE8mNvjDSMvtnrkMojN5 joerg@turingmachine"
  ];
}
