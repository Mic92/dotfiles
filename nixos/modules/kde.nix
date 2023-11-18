{ pkgs, config, ... }: {
  services.xserver.desktopManager.plasma5.enable = true;
  #services.xserver.displayManager.gdm.enable = true;

  # https://nixos.wiki/wiki/KDE#KMail_Renders_Blank_Messages
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (pkgs.lib.reverseList config.environment.profiles)}";
  };

  environment.systemPackages = with pkgs; [
    kmail
    kalendar
    ferdium
    firefox
    chromium
    pavucontrol
  ];
}
