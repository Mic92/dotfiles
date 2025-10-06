{ pkgs, self, ... }:
{
  environment.systemPackages = with pkgs; [
    # must have
    psmisc
    openssl
    binutils
    file
    wget
    direnv
    htop
    ripgrep
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

    self.packages.${pkgs.stdenv.hostPlatform.system}.direnv-instant
  ];
}
