{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = let
    scripts = pkgs.callPackage ./pkgs/scripts {};
  in
    with pkgs; [
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
      pkgconfig
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
