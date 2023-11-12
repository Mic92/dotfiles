{ pkgs
, self
, ...
}: {
  # Enable nix ld
  programs.nix-ld.enable = true;
  programs.nix-ld.package = self.inputs.nix-ld-rs.packages.${pkgs.hostPlatform.system}.nix-ld-rs;

  programs.nix-ld.libraries = with pkgs; [
    stdenv.cc.cc
    fuse3
    alsa-lib
    at-spi2-atk
    at-spi2-core
    atk
    cairo
    cups
    curl
    dbus
    expat
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libGL
    libappindicator-gtk3
    libdrm
    libnotify
    libpulseaudio
    libuuid
    libusb1
    xorg.libxcb
    libxkbcommon
    mesa
    nspr
    nss
    pango
    pipewire
    systemd
    icu
    openssl
    xorg.libX11
    xorg.libXScrnSaver
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXrandr
    xorg.libXrender
    xorg.libXtst
    xorg.libxkbfile
    xorg.libxshmfence
    zlib
  ];
}
