{ pkgs, config, ... }:

with pkgs;

let
  fuidshift = callPackage (import ./fuidshift.nix) {};
in {
  boot = {
    kernelPackages = linuxPackages;
    extraModulePackages = with config.boot.kernelPackages; [
      wireguard
    ];
  };
  environment.systemPackages = [
    ruby.devEnv
    fuidshift
    # must have
    psmisc
    p7zip
    openssl
    binutils
    file
    wget
    direnv
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
    go
    gcc
    rustc
    strace
    ltrace
    nethogs
    iotop
    gnumake
    pkgconfig
    cmake
    manpages
    netcat
    mtr
    nix-zsh-completions
    vim
    hdparm
    nftables
    dnsutils
    iperf3

    # additional
    mosh
    wireguard
    gnupg1compat
    socat
    whois
    ansible2
  ];
}
