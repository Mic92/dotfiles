{ pkgs, config, ... }:

{
  boot = {
    #kernelPackages = linuxPackages_latest;
    extraModulePackages = with config.boot.kernelPackages; [ wireguard ];
    zfs.enableUnstable = true;
  };

  environment.systemPackages = with pkgs; [
    vim
    #cntr
    qt5.qttools
    kmail
    kdeApplications.akonadi-mime
    kdeApplications.korganizer

    cryptsetup
    usbutils
    wireguard
    socat
    whois
    gnome3.defaultIconTheme
    hicolor_icon_theme

    # must have
    psmisc
    p7zip
    sipcalc
    iperf
    openssl
    binutils
    file
    wget
    htop
    ag
    lsof
    tcpdump
    tmux
    rsync
    git
    tig
    ruby.devEnv
    python
    python3
    nethogs
    iotop
    manpages
    dnsutils
    netcat
    mtr
    ntfs3g
  ];
}
