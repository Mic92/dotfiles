{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    # must have
    psmisc
    openssl
    binutils
    file
    wget
    direnv
    htop
    silver-searcher
    lsof
    tcpdump
    tmux
    rsync
    git
    tig
    python3
    strace
    ltrace
    nethogs
    iotop
    man-pages
    netcat
    mtr
    vim
    hdparm
    dnsutils

    # additional
    mosh
    socat
    whois
  ];
}
