{
  nix.sshServe.enable = true;
  nix.sshServe.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAf1/F4DuW7FO1gHRK5K7aWOH0HMvbGeWcWXa8FXuYzX binary-cache"
  ];
}
