{...}:
# build openstack image:
# $ nix-build '<nixpkgs/nixos>' -A config.system.build.novaImage --arg configuration "{ imports = [ ./nova-image.nix ]; }"
{
  imports = [
    <nixpkgs/nixos/maintainers/scripts/openstack/nova-image.nix>
    ./base-config.nix
  ];
}
