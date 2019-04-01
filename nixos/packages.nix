{ pkgs, config, ... }:

{
  boot = {
    #kernelPackages = pkgs.linuxPackages_latest;
    zfs.enableUnstable = true;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import <nur> { inherit pkgs; };
  };
  
  environment.systemPackages = with pkgs; [
    neovim
    nur.repos.mic92.cntr

    cryptsetup
    usbutils
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
    ruby.devEnv
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
