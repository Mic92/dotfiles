{ self, pkgs, ... }:
{
  # Install GUI apps via nix-casks and custom packages
  environment.systemPackages =
    (with self.inputs.nix-casks.packages.${pkgs.system}; [
      alt-tab
      ferdium
      firefox
      ungoogled-chromium
      signal
      ghostty
      via
      claude
      chatgpt-atlas
      secretive
    ])
    ++ [
      self.packages.${pkgs.system}.kdeconnect
    ];

  # Configure SSH to use Secretive for key management
  environment.etc."ssh/ssh_config.d/secretive.conf".text = ''
    Host *
      IdentityAgent ~/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
  '';

  # Ghostty terminal requires Nerd Fonts
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
