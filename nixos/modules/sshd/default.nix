{ config
, lib
, ...
}:
let
  cert = ./certs + "/${config.networking.hostName}-cert.pub";
in
{
  imports = [
    ../ssh.nix
  ];

  warnings = lib.optional (! builtins.pathExists cert) "No ssh certificate found at ${toString cert}";

  # srvos sets more sane defaults
  services.openssh = {
    enable = true;
    settings = lib.optionalAttrs (builtins.pathExists cert) {
      HostCertificate = toString cert;
    };
  };
}
