{pkgs, ...}:
with pkgs;

let
  myhunspell = (hunspellWithDicts (with hunspellDicts; [en-us en-gb-ise]));
in {
  boot = {
    kernelPackages = linuxPackages_4_11;
    extraModulePackages = with linuxPackages_4_11; [ bcc wireguard sysdig ];
    zfs.enableUnstable = true;
  };
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      hplip = pkgs.hplip.override { withPlugin = true; };
      #neovim = pkgs.neovim.override {
      #  vimAlias = true;
      #  extraPythonPackages = with python27Packages; [jedi];
      #  #python3Packages = python36Packages;
      #  extraPython3Packages = with python3Packages; [jedi requests2];
      #};
      networkd = stdenv.mkDerivation {
        name = "systemd-networkd";
        buildInputs = [ 
          linuxHeaders pkgconfig intltool gperf libcap kmod
          xz pam acl
          libuuid m4 glib libxslt libgcrypt libgpgerror
          libmicrohttpd kexectools libseccomp libffi audit lz4 libapparmor
          iptables gnu-efi
          gettext docbook_xsl docbook_xml_dtd_42 docbook_xml_dtd_45
          (python3.withPackages (pythonPackages: with pythonPackages; [ lxml ]))
          patchelf
        ];
        nativeBuildInputs = [ meson ninja glibcLocales ];
        src = fetchFromGitHub {
          owner = "Mic92";
          repo = "systemd";
          rev = "7ac926b7cf31cc8615078bb796a6724d4581888d";
          sha256 = "1xa9ls7dphdhfb0fgkd3r4afn4nylmz6mjvhh50rhpdqm0dl7sn8";
        };
        LC_ALL="en_US.utf8";

        configurePhase = ''
          patchShebangs .
          meson -D system-uid-max=499 -D system-gid-max=499 . build
          cd build
        '';
        buildPhase = "ninja systemd-networkd";
        installPhase = ''
          mkdir -p $out/{bin,lib}
          cp src/shared/libsystemd-shared-*.so $out/lib
          cp ./systemd-networkd $out/bin
          patchelf --set-rpath "$out/lib:\$ORIGIN" $out/bin/systemd-networkd
        '';
      };
    };
  };

  environment.systemPackages = [
    networkd
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
