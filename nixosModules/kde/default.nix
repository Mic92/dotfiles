{ pkgs, config, ... }:
{
  imports = [ ./calendar ];

  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  # https://wiki.nixos.org/wiki/KDE#KMail_Renders_Blank_Messages
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (
      pkgs.lib.reverseList config.environment.profiles
    )}";
  };

  environment.systemPackages = with pkgs; [
    ferdium
    librewolf
    firefox
    chromium
    pavucontrol
    bottles
    libnotify
    kwalletcli

    wl-clipboard # wl-copy / wl-paste
    (pkgs.callPackage ./choosers.nix { })
  ];
  programs.kdeconnect.enable = true;
}
