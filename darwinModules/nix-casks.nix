{ self, pkgs, ... }:
let
  cask = self.inputs.nix-casks.packages.${pkgs.stdenv.hostPlatform.system};
  myPkgs = self.packages.${pkgs.stdenv.hostPlatform.system};
  firefoxExtensions = pkgs.callPackages ../pkgs/firefox-extensions { };
in
{
  imports = [ ./casks.nix ];

  environment.casks = [
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
    cask.calibre
    myPkgs.kdeconnect
    (myPkgs.librewolf-macos.override {
      policies = import ../pkgs/librewolf-policies.nix { inherit firefoxExtensions; };
    })
    myPkgs.radicle-desktop
  ];

  # Configure SSH to use Secretive for key management
  environment.etc."ssh/ssh_config.d/secretive.conf".text = ''
    Host *
      IdentityAgent ~/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
  '';

  # Ghostty terminal requires Nerd Fonts
  fonts.packages = [ pkgs.nerd-fonts.fira-code ];
}
