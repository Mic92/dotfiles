{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.writeScriptBin "openvpn-tum" ''
      export PATH=$PATH:${pkgs.iproute2}/bin
      exec "${pkgs.openvpn}/bin/openvpn" ${./vpn-il1-standard.ovpn}
    '')
  ];
}
