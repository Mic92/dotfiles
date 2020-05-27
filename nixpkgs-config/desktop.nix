{ pkgs, lib, config, ... }:

let
  vscodeExtensions = (with pkgs.vscode-extensions; [
    bbenoist.Nix
    ms-python.python
    ms-azuretools.vscode-docker
    ms-vscode-remote.remote-ssh
    ms-vscode.cpptools
    vscodevim.vim
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
    name = "remote-ssh-edit";
    publisher = "ms-vscode-remote";
    version = "0.50.0";
    sha256 = "1b2lqd89vnynbzd3rss1jahc1zxs769s921rclgy1v7z1sd1kqxy";
  }];
in {
  imports = [
    ./common.nix
    ./modules/rust.nix
    ./modules/latex.nix
    ./modules/debugging.nix
    ./modules/bitwarden.nix
  ];

  manual.json.enable = true;

  fonts.fontconfig.enable = true;

  programs.emacs.imagemagick.enable = true;

  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "SauceCodePro Nerd Font Mono 8";
        alignment = "left";
        geometry = "0x5-3+29";
        corner_radius = "2";
        padding = "8";
        horizontal_padding = "8";
        frame_width = "1";
        frame_color = "#dbdbdb";
        markup = "full";
        transparency = "10";
      };
      urgency_low = {
        background = "#303030";
        foreground = "#888888";
        timeout = "10";
      };
      urgency_normal = {
        background = "#303030";
        foreground = "#c7c7c7";
        timeout = "10";
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = "0";
      };
    };
  };

  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = [ "network.target" "sound.target" ];
    Install.WantedBy = [ "default.target" ];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Service.Restart = "always";
  };

  services.syncthing.enable = true;

  home.packages = with pkgs; [
    #(vscode-with-extensions.override {
    #  inherit vscodeExtensions;
    #})

    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    twitter-color-emoji

    arandr
    xlibs.xkill
    signal-desktop
    nur.repos.mic92.pandoc-bin
    gnome3.defaultIconTheme
    hicolor_icon_theme
    graphicsmagick
    gimp
    firefox
    #chromium
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    urlview
    dino
    xorg.xev
    xorg.xprop
    xclip
    gpodder
    ncmpcpp
    xclip
    screen-message
    alacritty
    sshfsFuse
    sshuttle
    jq
    httpie
    pypi2nix
    go2nix
    nix-index
    gnupg1compat
    cheat
    tldr
    nixpkgs-pytools

    (mpv.override {
      scripts = [ mpvScripts.mpris ];
    })
    wmc-mpris
    playerctl
    youtube-dl

    isync
    mu
    # to fix xdg-open
    glib

    rubber
    (texlive.combine {
      inherit (texlive)
      scheme-full

      # awesome cv
      xetex
      unicode-math
      ucharcat
      collection-fontsextra
      fontspec

      collection-binextra
      collection-fontsrecommended
      collection-latex
      collection-latexextra
      collection-latexrecommended
      collection-langgerman
      siunitx
      bibtex
      tracklang
      IEEEtran
      algorithm2e;
    })
  ] ++ (with nur.repos.mic92; [
    speedscope
    inxi
    source-code-pro-nerdfonts
    ferdi
  ]);

  xdg.mimeApps = {
    enable = true;
    defaultApplications = let
      # take from the respective mimetype files
      images = [
        "image/bmp"
        "image/gif"
        "image/jpeg"
        "image/jpg"
        "image/pjpeg"
        "image/png"
        "image/tiff"
        "image/x-bmp"
        "image/x-gray"
        "image/x-icb"
        "image/x-ico"
        "image/x-png"
        "image/x-portable-anymap"
        "image/x-portable-bitmap"
        "image/x-portable-graymap"
        "image/x-portable-pixmap"
        "image/x-xbitmap"
        "image/x-xpixmap"
        "image/x-pcx"
        "image/svg+xml"
        "image/svg+xml-compressed"
        "image/vnd.wap.wbmp;image/x-icns"
      ];
      urls = [
        "text/html"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/about"
        "x-scheme-handler/unknown"
      ];
      documents = [
        "application/vnd.comicbook-rar"
        "application/vnd.comicbook+zip"
        "application/x-cb7"
        "application/x-cbr"
        "application/x-cbt"
        "application/x-cbz"
        "application/x-ext-cb7"
        "application/x-ext-cbr"
        "application/x-ext-cbt"
        "application/x-ext-cbz"
        "application/x-ext-djv"
        "application/x-ext-djvu"
        "image/vnd.djvu+multipage"
        "application/x-bzdvi"
        "application/x-dvi"
        "application/x-ext-dvi"
        "application/x-gzdvi"
        "application/pdf"
        "application/x-bzpdf"
        "application/x-ext-pdf"
        "application/x-gzpdf"
        "application/x-xzpdf"
        "application/postscript"
        "application/x-bzpostscript"
        "application/x-gzpostscript"
        "application/x-ext-eps"
        "application/x-ext-ps"
        "image/x-bzeps"
        "image/x-eps"
        "image/x-gzeps"
        "image/tiff"
        "application/oxps"
        "application/vnd.ms-xpsdocument"
        "application/illustrator"
      ];
      audioVideo = [
        "application/ogg"
        "application/x-ogg"
        "application/mxf"
        "application/sdp"
        "application/smil"
        "application/x-smil"
        "application/streamingmedia"
        "application/x-streamingmedia"
        "application/vnd.rn-realmedia"
        "application/vnd.rn-realmedia-vbr"
        "audio/aac"
        "audio/x-aac"
        "audio/vnd.dolby.heaac.1"
        "audio/vnd.dolby.heaac.2"
        "audio/aiff"
        "audio/x-aiff"
        "audio/m4a"
        "audio/x-m4a"
        "application/x-extension-m4a"
        "audio/mp1"
        "audio/x-mp1"
        "audio/mp2"
        "audio/x-mp2"
        "audio/mp3"
        "audio/x-mp3"
        "audio/mpeg"
        "audio/mpeg2"
        "audio/mpeg3"
        "audio/mpegurl"
        "audio/x-mpegurl"
        "audio/mpg"
        "audio/x-mpg"
        "audio/rn-mpeg"
        "audio/musepack"
        "audio/x-musepack"
        "audio/ogg"
        "audio/scpls"
        "audio/x-scpls"
        "audio/vnd.rn-realaudio"
        "audio/wav"
        "audio/x-pn-wav"
        "audio/x-pn-windows-pcm"
        "audio/x-realaudio"
        "audio/x-pn-realaudio"
        "audio/x-ms-wma"
        "audio/x-pls"
        "audio/x-wav"
        "video/mpeg"
        "video/x-mpeg2"
        "video/x-mpeg3"
        "video/mp4v-es"
        "video/x-m4v"
        "video/mp4"
        "application/x-extension-mp4"
        "video/divx"
        "video/vnd.divx"
        "video/msvideo"
        "video/x-msvideo"
        "video/ogg"
        "video/quicktime"
        "video/vnd.rn-realvideo"
        "video/x-ms-afs"
        "video/x-ms-asf"
        "audio/x-ms-asf"
        "application/vnd.ms-asf"
        "video/x-ms-wmv"
        "video/x-ms-wmx"
        "video/x-ms-wvxvideo"
        "video/x-avi"
        "video/avi"
        "video/x-flic"
        "video/fli"
        "video/x-flc"
        "video/flv"
        "video/x-flv"
        "video/x-theora"
        "video/x-theora+ogg"
        "video/x-matroska"
        "video/mkv"
        "audio/x-matroska"
        "application/x-matroska"
        "video/webm"
        "audio/webm"
        "audio/vorbis"
        "audio/x-vorbis"
        "audio/x-vorbis+ogg"
        "video/x-ogm"
        "video/x-ogm+ogg"
        "application/x-ogm"
        "application/x-ogm-audio"
        "application/x-ogm-video"
        "application/x-shorten"
        "audio/x-shorten"
        "audio/x-ape"
        "audio/x-wavpack"
        "audio/x-tta"
        "audio/AMR"
        "audio/ac3"
        "audio/eac3"
        "audio/amr-wb"
        "video/mp2t"
        "audio/flac"
        "audio/mp4"
        "application/x-mpegurl"
        "video/vnd.mpegurl"
        "application/vnd.apple.mpegurl"
        "audio/x-pn-au"
        "video/3gp"
        "video/3gpp"
        "video/3gpp2"
        "audio/3gpp"
        "audio/3gpp2"
        "video/dv"
        "audio/dv"
        "audio/opus"
        "audio/vnd.dts"
        "audio/vnd.dts.hd"
        "audio/x-adpcm"
        "application/x-cue"
        "audio/m3u"
      ];
    in {
      "x-scheme-handler/mailto" = [ "org.gnome.Evolution.desktop" ];
    } // (lib.genAttrs images (_: [ "org.gnome.eog.desktop" ]))
      // (lib.genAttrs urls (_: [ "firefox.desktop" ]))
      // (lib.genAttrs documents (_: [ "org.gnome.Evince.desktop" ]))
      // (lib.genAttrs audioVideo (_: [ "mpv.desktop" ]));
  };
}
