{pkgs, config, ...}:
with pkgs;

{
  boot = {
    kernelPackages = linuxPackages_latest;
    extraModulePackages = with config.boot.kernelPackages; [ bcc wireguard sysdig ];
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./overlays/mypackages) ];

  environment.systemPackages = [
    config.boot.kernelPackages.perf
    linuxPackages.bcc
    usbutils
    (sysdig.overrideDerivation (old: { dontStrip = true; }))
    wireguard
    socat
    whois

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
    strace
    ltrace
    nethogs
    iotop
    gnumake
    manpages
    dnsutils
    netcat
    mtr
    ntfs3g

    arc-icon-theme
    arc-theme
  ];
}
