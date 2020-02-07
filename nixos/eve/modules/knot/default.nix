{ pkgs, config, lib, ... }: let
  ip4 = config.networking.eve.ipv4.address;
  ip6 = lib.head config.networking.eve.ipv6.addresses;
in {
  imports = [
    ./whoami.nix
  ];
  services.knot = {
    enable = true;
    checkConfig = false;
    extraConfig = ''
      server:
        listen: ${ip4}@53
        listen: ${ip6}@53

      include: /var/lib/knot/knot-he-key.conf

      remote:
        - id: he_ip4
          address: 216.218.130.2
        - id: he_ip6
          address: 2001:470:100::2

      acl:
        - id: he_acl
          key: he1
          action: transfer

      zone:
        - domain: thalheim.io
          file: "${./thalheim.io.zone}"
          notify: [he_ip4, he_ip6]
          acl: [he_acl]
        - domain: higgsboson.tk
          file: "${./higgsboson.tk.zone}"
          notify: [he_ip4, he_ip6]
          acl: [he_acl]
    '';
  };
  systemd.services.knot = {
    serviceConfig.PermissionsStartOnly = true;
    preStart = ''
      install -m700 --owner $USER /var/src/secrets/knot-he-key.conf /var/lib/knot/knot-he-key.conf
    '';
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
