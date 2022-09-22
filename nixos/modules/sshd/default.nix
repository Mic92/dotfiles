{
  config,
  lib,
  ...
}: let
  cert = ./certs + "/${config.networking.hostName}-cert.pub";
in {
  imports = [
    ../ssh.nix
  ];

  warnings =
    lib.optional (! builtins.pathExists cert)
    "No ssh certificate found at ${toString cert}";

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    useDns = false;
    extraConfig = ''
      ${lib.optionalString (builtins.pathExists cert) "HostCertificate ${cert}"}
      # unbind gnupg sockets if they exists
      StreamLocalBindUnlink yes
    '';
  };
}
