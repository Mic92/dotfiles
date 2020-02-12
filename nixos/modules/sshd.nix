{
  imports = [ ./netdata.nix ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    useDns = false;
    # unbind gnupg sockets if they exists
    extraConfig = "StreamLocalBindUnlink yes";
  };

  services.netdata.portcheck.checks = {
    ssh.port = 22;
    ssh-legacy.port = 22022;
  };
}
