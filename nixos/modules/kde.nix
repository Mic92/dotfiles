{ pkgs, ... }: {
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.sddm.enable = true;

  environment.systemPackages = with pkgs; [
    gnome.adwaita-icon-theme
    hicolor-icon-theme

    qt5.qttools
    kmail
    kdeApplications.kleopatra
    kdeApplications.akonadi-mime
    kdeApplications.korganizer
    kdeApplications.kontact
    kdeApplications.kaddressbook
    kdeApplications.korganizer
    kdeconnect
    kdeApplications.spectacle
    kdeApplications.gwenview
    kdeApplications.dolphin
    kdeApplications.okular
    kdeApplications.akonadi-contacts
    kdeApplications.akonadi-mime
    kdeApplications.akonadi-calendar
    kdeFrameworks.networkmanager-qt
    lxqt.pavucontrol-qt
  ];
}
