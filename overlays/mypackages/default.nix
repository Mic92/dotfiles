self: super:

{
  hplip = super.hplip.override { withPlugin = true; };
  #neovim = pkgs.neovim.override {
  #  vimAlias = true;
  #  extraPythonPackages = with python27Packages; [jedi];
  #  #python3Packages = python36Packages;
  #  extraPython3Packages = with python3Packages; [jedi requests2];
  #};

  mynix = super.nixUnstable.overrideDerivation (old: {
    src = self.fetchFromGitHub {
      owner = "NixOS";
      repo = "nix";
      rev = "a7e55151a8d45d987ca42ba318c44ed3ccdeecca";
      sha256 = "0qnnc8wbh55j2mpnywvj22ajcqfcdfismxbgkix45hq4nm5lkb1j";
    };
    enableParallelBuilding = true;
  });

  mysystemd = super.systemd.overrideDerivation(old: {
    src = self.fetchFromGitHub {
      owner = "Mic92";
      repo = "systemd";
      rev = "17b7553d68519399c2fefc8bbed234f86a240992";
      sha256 = "1gbi9aq1gq3il8256ffa2s4nyp26qzfyzsmvyr0kqfdmxb9pjj39";
    };
    preConfigure = old.preConfigure + ''
      substituteInPlace src/network/networkd-manager.c \
        --replace /usr/lib/systemd/network $out/lib/systemd/network
    '';
  });

  networkd = super.stdenv.mkDerivation {
    name = "systemd-networkd";
    buildInputs = with self; [
      linuxHeaders pkgconfig intltool gperf libcap kmod
      xz pam acl
      libuuid m4 glib libxslt libgcrypt libgpgerror
      libmicrohttpd kexectools libseccomp libffi audit lz4 libapparmor
      iptables gnu-efi
      gettext docbook_xsl docbook_xml_dtd_42 docbook_xml_dtd_45
      (python3.withPackages (pythonPackages: with pythonPackages; [ lxml ]))
      patchelf
    ];
    nativeBuildInputs = with self; [ meson ninja glibcLocales ];
    src = self.fetchFromGitHub {
      owner = "Mic92";
      repo = "systemd";
      rev = "9f11187dae637c3184922276dd809a0ebcb9cb69";
      sha256 = "05nknky9ga2spfmid6gwplinn0iyc7rm63dxj7fmnb8yxdywqq1q";
    };
    LC_ALL="en_US.utf8";

    configurePhase = ''
      patchShebangs .
      substituteInPlace src/network/networkd-manager.c \
        --replace /usr/lib/systemd/network $out/lib/systemd/network
      meson -D system-uid-max=499 -D system-gid-max=499 . build
      cd build
      '';
    buildPhase = "ninja systemd-networkd";
    installPhase = ''
      mkdir -p $out/{bin,lib/systemd}
      cp src/shared/libsystemd-shared-*.so $out/lib
      cp -r ../network $out/lib/systemd/network
      cp ./systemd-networkd $out/bin
      patchelf --set-rpath "$out/lib:\$ORIGIN" $out/bin/systemd-networkd
    '';
  };
}
