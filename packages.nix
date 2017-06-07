{pkgs, ...}:
with pkgs;

{
  boot = {
    kernelPackages = linuxPackages_4_11;
    extraModulePackages = with linuxPackages_4_11; [ bcc wireguard sysdig ];
    zfs.enableUnstable = true;
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./overlays/mypackages) ];

  environment.systemPackages = [
    mysystemd
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
    libxml2
    openssl
    zlib
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
    cmake
    manpages
    dnsutils
    netcat
    mtr
    nix-zsh-completions
    ntfs3g
  ];
}
