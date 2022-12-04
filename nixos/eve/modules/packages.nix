{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs; [
    ruby.devEnv
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
    python
    python3
    go
    gcc
    rustc
    strace
    ltrace
    nethogs
    iotop
    gnumake
    pkg-config
    cmake
    man-pages
    netcat
    mtr
    vim
    hdparm
    dnsutils

    # additional
    mosh
    gnupg1compat
    socat
    whois
  ];
}
