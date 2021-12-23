{ pkgs, ... }:
let
  resolved = pkgs.systemd.overrideAttrs (old: {
    buildPhase = ''
      ninja systemd-resolved
    '';
    # resolved downgrade to udp/53 which is a problem in flacky networks with servers that don't do udp/53
    patches = [
      ./0001-networkd-don-t-downgrade-dnsovertls.patch
    ];
    installPhase = ''
      install -D systemd-resolved $out/bin/systemd-resolved
      shared=$(echo src/shared/libsystemd-shared-*.so)
      install -D $shared $out/lib/$(basename $shared)
    '';
    postFixup = "";
    outputs = [ "out" ];
  });
in
{
  systemd.network.enable = true;
  systemd.services.systemd-resolved.serviceConfig = {
    ExecStart = [ "" "!!${resolved}/bin/systemd-resolved" ];
  };

  services.resolved.enable = true;
  networking.nameservers = [
    "88.99.244.96#dns.thalheim.io"
    "2a01:4f8:10b:49f::1#dns.thalheim.io"
  ];
  services.resolved.extraConfig = ''
    DNSOverTLS=yes
  '';
  # dns.thalheim.io performs dnssec already
  services.resolved.dnssec = "false";

  # often hangs
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking.dhcpcd.enable = false;
}
