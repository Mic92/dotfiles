# nix-build '<nixpkgs/nixos>' -A config.system.build.sdImage -I nixos-config=./rpi4-image.nix
{ ... }: {
  nixpkgs.localSystem.system = "aarch64-linux";
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/sd-image-raspberrypi4.nix>
    ./base-config.nix
  ];

  # maurice
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH756wA1zmO+FWP1/WJTwezIxBmDQp5ocS5kyA8jqLRv"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzeGqwmfgxmpLnZyRfMuQVVboNkm0ruPAzHdxgu/31GS8M8qyh6gxqg80JOhFejz8FiHSbbFn5gB/QkBWpFpLf3fEJnvxfmSTXLAVjfcDwTiLghaPW3+xAktzpA5E45ITRc6ljOXHp8FDi22piqqg9hOrQiiZmnaYbwLhH8K3vAMySYzp8XlAw7LacXfTyyaZUD3Zx6hw4rxxkN8QhHrJABgkV/DoY3JbbpXspejxU5IBuem/B/iU/NjnXJ7VGleJ3uvUqRQjEq3eWjRspJ1ctW1ARPzXC5uMqJXyEJlrapJD1dPOPYb57TQg/FKofTfvmxAPp+QaUiqbsk8OiL2PB"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEoquCN7wONwcfAFr08V4zWzXoAbJ3WbZWxfeFc/TLA2"
  ];
}
