{pkgs, config, ...}:
with pkgs;

{
  boot = {
    kernelPackages = linuxPackages_latest;
    extraModulePackages = with config.boot.kernelPackages; [ wireguard ];
    zfs.enableUnstable = true;
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [
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
    pkgconfig
    openssl
    binutils
    file
    wget
    #neovim
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
    go
    gcc
    nethogs
    iotop
    manpages
    dnsutils
    netcat
    mtr
    ntfs3g
  ];
}
