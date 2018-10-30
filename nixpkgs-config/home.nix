{ pkgs, ... }:



with pkgs;

let
  mybrowser = firefox;

  isDesktop = true;

  latexApps = [
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
  ];

  desktopApps = [
    radare2
    rambox

    league-of-moveable-type
    dejavu_fonts
    ubuntu_font_family
    unifont
    gnome3.defaultIconTheme
    hicolor_icon_theme
    graphicsmagick
    sshfsFuse
    sshuttle
    dropbox
    gimp
    mpv
    youtube-dl
    mybrowser
    chromium
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
    keepassx-community
    gpodder
    ncmpcpp
    xclip
    screen-message
    alacritty
    sublime3
  ] ++ (with nur.repos.mic92; [
    inxi
    conky-symbols
    inconsolata-nerdfonts
    gdbgui
    gdb-dashboard
  ]);

  nixDevApps = [
    nix-prefetch-scripts
    pypi2nix
    go2nix
    mercurial # go2nix
    nox
    nur.repos.mic92.nix-review-unstable
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
  fonts.fontconfig.enableProfileFonts = true;

  home.packages = ((lib.optionals isDesktop desktopApps)
  #++ (lib.optionals isDesktop rustApps)
  ++ (lib.optionals isDesktop latexApps)
  #++ pythonDataLibs
  ++ debuggingApps
  ++ debuggingBasicsApps
  ++ nixDevApps
  ++ [
    myvim
    # python language server + plugins
    (python36.withPackages(ps: [ ps.pyls-mypy ps.pyls-isort ps.pyls-black ]))

    rustup

    tmux
    htop
    psmisc
    lesspipe
    expect
    gitAndTools.hub
    gitAndTools.tig
    jq
    httpie
    cloc
    mosh
    cheat
    tldr
    gnupg1compat
    direnv
    fzf
    exa
    fd
    bat
  ]);

  services.syncthing.enable = isDesktop;

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
}
