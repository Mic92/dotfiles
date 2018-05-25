{ pkgs, ... }:



with pkgs;

let
  mybrowser = firefox;

  myNodePackages = (callPackages ./write-good/composition.nix {});

  latexApps = [
    rubber
    myNodePackages.write-good

    (texlive.combine {
      inherit (texlive)
      scheme-basic

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
      IEEEtran;
    })
  ];


  rubyApps = [ bundler bundix rubocop ];

  desktopApps = [
    qt5.qttools
    (gdbgui.overrideAttrs (old: {
      patches = [ ./gdbgui.patch ];
    }))
    (stdenv.mkDerivation rec {
      name = "ConkySymbols";
      src = fetchzip {
        url = "https://github.com/Mic92/awesome-dotfiles/releases/download/download/ConkySymbols.ttf.tar.gz";
        sha256 = "08xhavw9kgi2jdmpzmxalcpbnzhng1g3z69v9s7yax4gj0jdlss5";
      };
      buildCommand = ''
        install -D $src/ConkySymbols.ttf "$out/share/fonts/truetype/ConkySymbols.ttf"
      '';
    })
    myNodePackages.typescript-language-server
    radare2
    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    #emojione
    (stdenv.mkDerivation rec {
      name = "inconsolata-nerdfont-${version}";
      version = nerdfonts.version;
      src = fetchurl {
        name = "inconsolata.otf";
        url = "https://github.com/ryanoasis/nerd-fonts/raw/${version}/patched-fonts/Inconsolata/complete/Inconsolata%20Nerd%20Font%20Complete%20Mono.otf";
        sha256 = "0dpi27lpag46diynw7z0yfcqsp87mf4skzz1pdq18x1wxfc3nymz";
      };
      buildCommand = ''
        install -D $src "$out/share/fonts/opentype/Inconsolata Nerd Font Complete.otf"
      '';
    })

    gnome3.defaultIconTheme
    hicolor_icon_theme

    screen
    #remmina
    graphicsmagick
    bench
    sshfsFuse
    sshuttle
    libreoffice-fresh
    dropbox
    #android-studio
    gimp
    #inkscape
    mpd
    mpv
    youtube-dl
    mybrowser
    chromium
    thunderbird
    transmission_gtk
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    scrot
    urlview
    dino
    arandr
    lxappearance
    xorg.xev
    xorg.xprop
    xclip
    xautolock
    keepassx-community
    pavucontrol
    evince
    pcmanfm
    gpodder
    valauncher
    ncmpcpp
    xclip
    screen-message
    scrot
    alacritty
  ] ++ (with gnome3; [
    gvfs
    eog
    sublime3
  ]);

  nixDevApps = [
    nix-prefetch-scripts
    pypi2nix
    go2nix
    mercurial # go2nix
    nox
    nix-review
  ];

  debuggingBasicsApps = [
    gdb
    strace
  ];

  debuggingApps = [ binutils gperftools valgrind ];

  rustApps = [
    rustc
    cargo
    rustfmt
    rustracer
    (writeScriptBin "rust-doc" ''
       #! ${stdenv.shell} -e
       exec "${mybrowser}" "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];

  pythonDataLibs = with python3Packages; [
    ipython
    numpy
    scipy
    matplotlib
    pandas
    seaborn
  ];

  dotfilesPath = file: ".homesick/repos/dotfiles/home/${file}";

in {
  programs.git = {
    enable = true;
    userEmail = "joerg@thalheim.io";
    userName = "Joerg Thalheim";
    package = gitFull;
    signing = {
      #signByDefault = builtins.pathExists ../../.gnupg/S.gpg-agent;
      key = "CA4106B8D7CC79FA";
    };
  };
 
  fonts.fontconfig.enableProfileFonts = true;

  home.packages = ([]
      ++ desktopApps
      ++ latexApps
      #++ rubyApps
      #++ rustApps
      #++ pythonDataLibs
      ++ debuggingApps
      ++ debuggingBasicsApps
      ++ nixDevApps
      ++ [
        myvim

        # python language server + plugins
        (python3.withPackages(ps: [ps.pyls-mypy ps.pyls-isort ]))

        nodePackages.jsonlint

        tmux
        htop
        psmisc
        lesspipe
        expect
        gitAndTools.diff-so-fancy
        gitAndTools.hub
        gitAndTools.git-crypt
        gitAndTools.git-extras
        gitAndTools.tig
        jq
        httpie
        cloc
        mosh
        cheat
        gnupg1compat
        direnv
        fzf
        exa
        fd
      ]);
}
