{ ... }:
# build openstack image:
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.novaImage --arg configuration "{ imports = [ ./nova-image.nix ]; }"
{
  imports = [
    <nixpkgs/nixos/maintainers/scripts/openstack/nova-image.nix>
    ./base-config.nix
  ];

  # Automatically log in at the virtual consoles.
  services.getty.autologinUser = "root";
  # Some more help text.
  services.getty.helpLine = ''
    The "root" account has an empty password.
  '';
  # Allow the user to log in as root without a password.
  users.users.root.initialHashedPassword = "";
}
