{ pkgs, config, ... }:

with pkgs;

{
  boot = {
    kernelPackages = linuxPackages;
    extraModulePackages = with config.boot.kernelPackages; [
      wireguard
    ];
  };
  environment.systemPackages = let
    scripts = stdenv.mkDerivation {
      name = "eve-scripts";
      src = ./scripts;
      buildInputs = [ ruby bash python3 ];
      nativeBuildInputs = [ makeWrapper ];
      installPhase = ''
        mkdir -p $out/bin
        install -D --target $out/bin *
      '';
    };
  in [
    scripts

    ruby.devEnv
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
    gnupg1compat
    socat
    whois
  ];
}
