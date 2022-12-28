{
  virtualisation.libvirtd.enable = true;
  users.extraUsers.joerg.extraGroups = [ "libvirtd" ];
  networking.firewall.checkReversePath = false;
}
