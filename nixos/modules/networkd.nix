{ pkgs, ... }: let
  # resolved with support TLS over DNS server with hostname-based certificates
  resolved = pkgs.systemd.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "systemd";
      repo = "systemd";
      rev = "fbc6d1716fc74d5475146cb6a98a849ade1e5ade";
      sha256 = "0r76djh4y0v4lb0djlvi5imm9a835b3prh8m7jfp6ckkcw2c4kab";
    };
    buildPhase = ''
      ninja systemd-resolved
    '';
    patches = [];
    installPhase = ''
      install -D systemd-resolved $out/bin/systemd-resolved
      install -D src/shared/libsystemd-shared-245.so $out/lib/libsystemd-shared-245.so
      set -x
    '';
    postFixup = "";
    outputs = [ "out" ];
  });
in {
  systemd.network.enable = true;
  systemd.services.systemd-resolved.serviceConfig = {
    ExecStart = [ "" "!!${resolved}/bin/systemd-resolved" ];
  };

  services.resolved.enable = true;
  networking.nameservers = [
    "95.216.112.61#dns.thalheim.io"
    "2a01:4f9:2b:1605::1#dns.thalheim.io"
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
