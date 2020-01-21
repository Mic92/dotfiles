{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    socat
    whois

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
    python
    python3
    strace
    nethogs
    iotop
    manpages
    dnsutils
    netcat
    mtr
 ];
}
