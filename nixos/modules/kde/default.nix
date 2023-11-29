{ pkgs, config, ... }: {
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # https://nixos.wiki/wiki/KDE#KMail_Renders_Blank_Messages
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (pkgs.lib.reverseList config.environment.profiles)}";
  };

  # I see weird power-throttling when this is enabled?
  # power-profiles-daemon should already be doing this.
  services.thermald.enable = false;

  environment.systemPackages = with pkgs; [
    kmail
    kalendar
    ferdium
    firefox
    chromium
    pavucontrol
    bottles

    wl-clipboard # wl-copy / wl-paste
    (pkgs.callPackage ./audio-chooser.nix { })
  ];
}
