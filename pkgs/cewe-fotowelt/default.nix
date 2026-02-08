{
  lib,
  stdenv,
  bash,
  libarchive,
  makeWrapper,
  autoPatchelfHook,
  libx11,
  libxext,
  libxrender,
  libxcomposite,
  libxi,
  libxcb,
  libxcb-cursor,
  libxkbfile,
  snappy,
  gst_all_1,
  libGL,
  fontconfig,
  freetype,
  dbus,
  cups,
  alsa-lib,
  pulseaudio,
  systemd,
  libxkbcommon,
  wayland,
  xdg-utils,
  fetchurl,
  libheif,
  nss,
  nspr,
  libgbm,
}:

let
  srcsJson = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation rec {
  pname = "cewe-fotowelt";
  inherit (srcsJson) version;

  srcs = map (src: fetchurl { inherit (src) url hash; }) srcsJson.sources;

  sourceRoot = ".";

  nativeBuildInputs = [
    libarchive
    makeWrapper
    autoPatchelfHook
  ];

  buildInputs = [
    # Shell
    bash

    # X11 libraries
    libx11
    libxext
    libxrender
    libxcomposite
    libxi
    libxcb
    libxcb-cursor
    libxkbcommon
    libxkbfile

    # Graphics
    libGL
    libgbm

    # System libraries
    fontconfig
    freetype
    dbus
    cups.lib
    alsa-lib
    pulseaudio
    systemd
    wayland

    # Media libraries
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-libav
    libheif

    # Compression
    snappy

    # Security
    nss
    nspr
  ];

  unpackPhase = ''
    runHook preUnpack

    # Extract archives in parallel using xargs
    echo "Extracting archives in parallel..."
    printf '%s\n' $srcs | xargs -n1 -P$(nproc) -I{} sh -c 'echo "Extracting {}"; bsdtar -xf "{}"'

    echo "All archives extracted"

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    # Create installation directory
    mkdir -p $out/opt/cewe-fotowelt

    # Copy all extracted files
    cp -r * $out/opt/cewe-fotowelt/

    # Fix shebang in startAutoBookService script
    substituteInPlace $out/opt/cewe-fotowelt/Resources/autobookservice/startAutoBookService \
      --replace-fail '#!/bin/bash' '#!${bash}/bin/bash'

    # Make executables actually executable
    chmod +x "$out/opt/cewe-fotowelt/CEWE Fotowelt" \
             "$out/opt/cewe-fotowelt/CEWE Fotoschau" \
             "$out/opt/cewe-fotowelt/autoBookEventClassifier" \
             "$out/opt/cewe-fotowelt/AutoBookService" \
             "$out/opt/cewe-fotowelt/faceRecognition" \
             "$out/opt/cewe-fotowelt/ffmpeg" \
             "$out/opt/cewe-fotowelt/ffprobe" \
             "$out/opt/cewe-fotowelt/gpuprobe" \
             "$out/opt/cewe-fotowelt/IconExtractor" \
             "$out/opt/cewe-fotowelt/QtWebEngineProcess" \
             "$out/opt/cewe-fotowelt/regedit" \
             "$out/opt/cewe-fotowelt/updater.pl" \
             "$out/opt/cewe-fotowelt/Resources/autobookservice/startAutoBookService" 2>/dev/null || true

    # Create bin directory
    mkdir -p $out/bin

    # The main executable is "CEWE Fotowelt" with a space
    mainExe="$out/opt/cewe-fotowelt/CEWE Fotowelt"

    if [ -f "$mainExe" ]; then
      makeWrapper "$mainExe" "$out/bin/cewe-fotowelt" \
        --prefix LD_LIBRARY_PATH : "$out/opt/cewe-fotowelt:${lib.makeLibraryPath buildInputs}" \
        --prefix PATH : "${lib.makeBinPath [ xdg-utils ]}"
    else
      echo "ERROR: Main executable not found at: $mainExe"
      exit 1
    fi

    # Create desktop file
    mkdir -p $out/share/applications
    cat > $out/share/applications/cewe-fotowelt.desktop <<EOF
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=CEWE Fotowelt
    Comment=Create photo books and photo products
    Exec=$out/bin/cewe-fotowelt %u
    Icon=cewe-fotowelt
    Categories=Graphics;Photography;
    MimeType=application/x-hps-${version}-mcf;
    EOF

    # Extract and install icon if present
    #iconFile=$(find $out/opt/cewe-fotowelt -name "*.ico" -o -name "*.png" | grep -i cewe | head -1)
    #if [ -n "$iconFile" ]; then
    #  mkdir -p $out/share/pixmaps
    #  cp "$iconFile" $out/share/pixmaps/cewe-fotowelt.png 2>/dev/null || \
    #  cp "$iconFile" $out/share/pixmaps/cewe-fotowelt.ico
    #fi

    runHook postInstall
  '';

  # Ensure all ELF files are properly patched
  dontStrip = true;

  # Configure auto-patchelf
  autoPatchelfIgnoreMissingDeps = true;

  # Add the output directory to rpath for internal libraries
  postFixup = ''
    # Create symlinks for the versioned libraries
    echo "Creating library symlinks..."
    cd $out/opt/cewe-fotowelt

    # Handle Qt libraries specially - they need .so.6 symlinks
    for lib in libQt6*.so.*.*; do
      if [ -f "$lib" ]; then
        # Extract base name and version parts
        base=$(echo "$lib" | sed 's/\.so\..*//')
        version=$(echo "$lib" | sed 's/.*\.so\.//')
        
        # Create .so.6 symlink (Qt6 libraries expect this)
        ln -sf "$lib" "$base.so.6"
        # Create .so symlink
        ln -sf "$lib" "$base.so"
        
        echo "Created symlinks for $lib -> $base.so.6, $base.so"
      fi
    done

    # Handle OpenCV libraries - they need .so.4.5 symlinks
    for lib in libopencv*.so.4.5.2; do
      if [ -f "$lib" ]; then
        base=$(echo "$lib" | sed 's/\.so\..*//')
        
        # Create .so.4.5 symlink
        ln -sf "$lib" "$base.so.4.5"
        # Create .so.4 symlink if not exists
        [ ! -e "$base.so.4" ] && ln -sf "$lib" "$base.so.4"
        # Create .so symlink if not exists
        [ ! -e "$base.so" ] && ln -sf "$lib" "$base.so"
        
        echo "Created symlinks for $lib -> $base.so.4.5"
      fi
    done

    # Handle other libraries
    for lib in *.so.*; do
      if [ -f "$lib" ] && [[ ! "$lib" =~ ^libQt6 ]] && [[ ! "$lib" =~ ^libopencv ]]; then
        base=$(echo "$lib" | sed 's/\.so\..*//')
        ln -sf "$lib" "$base.so" 2>/dev/null || true
        
        # For versioned libraries like libCWAPM.so.0.1.0, also create .so.0
        if [[ "$lib" =~ \.so\.[0-9]+\.[0-9]+\.[0-9]+ ]]; then
          major=$(echo "$lib" | sed 's/.*\.so\.\([0-9]*\)\..*/\1/')
          ln -sf "$lib" "$base.so.$major" 2>/dev/null || true
        fi
      fi
    done

    cd - > /dev/null

    # Update RPATH for all ELF files
    echo "Updating RPATH for all ELF files..."
    for f in $out/opt/cewe-fotowelt/* $out/opt/cewe-fotowelt/*/*; do
      if [ -f "$f" ] && file "$f" | grep -q "ELF"; then
        patchelf --set-rpath "$out/opt/cewe-fotowelt:\$ORIGIN:\$ORIGIN/lib:\$ORIGIN/../lib:${lib.makeLibraryPath buildInputs}" "$f" 2>/dev/null || true
      fi
    done
  '';

  meta = with lib; {
    description = "CEWE photo book software";
    longDescription = ''
      CEWE Fotowelt is a photo book creation and printing software that allows
      you to create photo books, calendars, and other photo products.
    '';
    homepage = "https://www.cewe.de/";
    license = licenses.unfree;
    platforms = [ "x86_64-linux" ];
    maintainers = [ ];
  };
}
