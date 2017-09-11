{ ... }:
{
  boot.supportedFilesystems = [ "zfs" ];
  networking.firewall.enable = false;
  services.openssh = {
    enable = true;
    startWhenNeeded = true;
  };
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
  ];
}
