{pkgs, ...}:
with pkgs;

let
  myhunspell = (hunspellWithDicts (with hunspellDicts; [en-us en-gb-ise]));
in {
  boot = {
    kernelPackages = linuxPackages_4_10;
    extraModulePackages = with linuxPackages_4_10; [ bcc wireguard sysdig ];
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
        buildInputs = [ linuxHeaders pkgconfig intltool gperf libcap kmod xz pam acl
          libuuid m4 glib libxslt libgcrypt libgpgerror
          libmicrohttpd kexectools libseccomp libffi audit lz4 libapparmor
          iptables gnu-efi file
          autoreconfHook gettext docbook_xsl docbook_xml_dtd_42 docbook_xml_dtd_45
          idutils patchelf
        ];
        src = fetchFromGitHub {
          owner = "Mic92";
          repo = "systemd";
          rev = "4bad9b7ef9ffa8e98d168119bf87f7bf69286531";
          sha256 = "17a7b7zmrzkx0yf6c38yq1nkvv88qd8qgihbans5i6cm4ryf27yy";
        };
        enableParallelBuilding = true;
        makeFlags = ["ID" "systemd-networkd"];
        preConfigure = "./autogen.sh";
        installPhase = ''
          mkdir -p $out/lib $out/bin
          cp .libs/*.so $out/lib/
          cp .libs/systemd-networkd $out/bin/
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
