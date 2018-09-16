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

        substituteInPlace $out/bin/lxc-destroy \
          --replace '@lxc@' "${lxc}"

        makeWrapper ${pkgs.lxc}/bin/lxc-attach $out/bin/lxc-attach \
          --run "cd /root" \
          --set HOME /root \
          --add-flags "--clear-env --keep-var TERM --keep-var HOME"
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
    wireguard
    gnupg1compat
    socat
    whois
    ansible2
  ];
}
