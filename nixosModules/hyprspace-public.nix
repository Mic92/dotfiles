{
  imports = [ ./hyprspace.nix ];

  services.hyprspace.blockRfc1918Addresses = true;
}
