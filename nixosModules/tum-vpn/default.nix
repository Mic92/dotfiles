{ pkgs, lib, ... }:
{
  environment.systemPackages = [
    (pkgs.writeScriptBin "openvpn-tum" ''
      ${lib.optionalString pkgs.stdenv.isLinux ''
        export PATH=$PATH:${pkgs.iproute2}/bin
      ''}
      exec "${pkgs.openvpn}/bin/openvpn" ${./vpn-il1-standard.ovpn}
    '')
  ];
}
