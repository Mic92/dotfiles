{ self, pkgs, ... }:
let
  cask = self.inputs.nix-casks.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  # Install GUI apps via nix-casks and custom packages
  environment.systemPackages = [
    cask.alt-tab
    cask.ferdium
    cask.ungoogled-chromium
    cask.signal
    cask.ghostty
    cask.via
    cask.claude
    cask.chatgpt-atlas
    cask.secretive
    cask.gather
    cask.inkscape
    self.packages.${pkgs.stdenv.hostPlatform.system}.kdeconnect
    self.packages.${pkgs.stdenv.hostPlatform.system}.librewolf-macos
    self.packages.${pkgs.stdenv.hostPlatform.system}.radicle-desktop
  ];

  # Configure SSH to use Secretive for key management
  environment.etc."ssh/ssh_config.d/secretive.conf".text = ''
    Host *
      IdentityAgent ~/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
  '';

  # Ghostty terminal requires Nerd Fonts
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
