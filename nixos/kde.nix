{ pkgs, ... }: {

  services.xserver.desktopManager.plasma5.enable = true;

  environment.systemPackages = with pkgs; [
    gnome3.defaultIconTheme
    hicolor_icon_theme

    qt5.qttools
    kmail
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
