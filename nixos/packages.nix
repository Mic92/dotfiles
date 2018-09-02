{ pkgs, config, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = with config.boot.kernelPackages; [ wireguard ];
    zfs.enableUnstable = true;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import <nur> { inherit pkgs; };
  };
  
  environment.systemPackages = with pkgs; [
    vim

    nur.repos.mic92.cntr

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
