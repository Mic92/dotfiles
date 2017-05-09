with import <nixpkgs> {};
let
  nvim = pkgs.neovim.override {
    vimAlias = true;
    configure.packages.nixbundle = with pkgs.vimPlugins; {
      # loaded on launch
      start = with vimPlugins; [ 
        youcompleteme
        syntastic
        gitgutter
        airline
        nerdtree
        colors-solarized
        vim-go
        vim-scala
        vim-polyglot
        syntastic
        nerdcommenter
        editorconfig-vim
        easymotion
        ctrlp
        rust-vim
        vim-trailing-whitespace
        pony-vim-syntax
        vim-css-color
      ];
    };
  };
  officeCommand = alias: exe: (stdenv.mkDerivation {
    buildInputs = [ wrapGAppsHook ];
    name = alias;
    src = null;
    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cat > $out/bin/${alias} <<'EOF'
#! ${pkgs.stdenv.shell} -e
export WINEPREFIX=~/.wineprefix/office2010
export PATH=${samba}/bin:$PATH
exec "${wineUnstable}/bin/wine" "$WINEPREFIX/drive_c/Program Files/Microsoft Office/Office14/${exe}"
EOF
      chmod +x $out/bin/${alias}
    '';
  });

  vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = "source ~/.vimrc";
    vimrcConfig.packages.nixbundle = with pkgs.vimPlugins; {
      # loaded on launch
      start = [ 
        youcompleteme
        #deoplete-nvim
        #deoplete-jedi
        #clang_complete
        syntastic
        gitgutter
        airline
        nerdtree
        colors-solarized
        vim-go
        vim-scala
        vim-polyglot
        syntastic
        # delimitMate
        editorconfig-vim
        ctrlp
        rust-vim
        # vim-trailing-whitespace
      ];
    };
  };

  rubyEnv = pkgs.loadRubyEnv ./.bundix/definition.nix {
    paths = with pkgs; [ bundler bundix rubocop ];
  };

  desktopApps = [
    dropbox
    #android-studio
    gimp
    inkscape
    mpd
    mpv
    firefox
    chromium
    thunderbird
    transmission_gtk
    rxvt_unicode-with-plugins
    aspell
    aspellDicts.de
    aspellDicts.fr
    aspellDicts.en
    hunspell
    hunspellDicts.en-gb-ise
    scrot
    gajim
    arandr
    lxappearance
    xorg.xev
    xorg.xprop
    xclip
    copyq
    xautolock
    i3lock
    zeroad
    keepassx-community
    pavucontrol
    evince
    pcmanfm
    gpodder
    valauncher
    youtube-dl
    ncmpcpp
    xclip
    screen-message
    scrot
  ] ++ (with gnome3; [
    gvfs
    eog
    gedit
    gnome_themes_standard
    adwaita-icon-theme
  ]);

  pythonLibs = with pythonPackages; [
    ipython
    numpy
    scipy
    matplotlib
    pandas
    seaborn
  ];

  rust = [
    rustup
    rustfmt
    rustracer
    (pkgs.writeScriptBin "rust-doc" ''
       #! ${pkgs.stdenv.shell} -e
       browser="$BROWSER"
       if [ -z "$browser" ]; then
         browser="$(type -P xdg-open || true)"
         if [ -z "$browser" ]; then
           browser="$(type -P w3m || true)"
           if [ -z "$browser" ]; then
             echo "$0: unable to start a web browser; please set \$BROWSER"
             exit 1
           fi
         fi
       fi
       exec "$browser" "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];

  nixDev = [
    nix-prefetch-scripts
    pypi2nix
    go2nix
    bundix
    nox
    nix-repl
  ];

  latex = [
    rubber
    (texlive.combine {
      inherit (texlive)
      scheme-basic

      # awesome cv
      xetex
      xetex-def
      unicode-math
      ucharcat
      collection-fontsextra
      fontspec

      collection-binextra
      collection-fontsrecommended
      collection-genericrecommended
      collection-latex
      collection-latexextra
      collection-latexrecommended
      collection-science
      collection-langgerman
      IEEEtran;
    })
  ];
in {
  allowUnfree = true;
  pulseaudio = true;
  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };
  packageOverrides = pkgs: with pkgs; {
    #pandas = pkgs.python3Packages.pandas.overridePythonPackage(old: rec {
    #  version = "0.19.1";
    #  src =  pkgs.python3Packages.fetchPypi {
    #    pname = "pandas";
    #    inherit version;
    #    sha256 = "08blshqj9zj1wyjhhw3kl2vas75vhhicvv72flvf1z3jvapgw295";
    #  };
    #});
    #st = (st.overrideDerivation (old: { dontStrip = true; })).override ({
    #    libXft = xorg.libXft.overrideDerivation (old: {
    #      dontStrip = false;
    #    });
    #});
    all = buildEnv {
      inherit ((import <nixpkgs/nixos> {}).config.system.path)
        pathsToLink ignoreCollisions postBuild;
      extraOutputsToInstall = [ "man" ];
      name = "all";
      paths = desktopApps
        ++ rust
        ++ pythonLibs
        ++ nixDev
        ++ [
          vim
          gitAndTools.diff-so-fancy
          gitAndTools.hub
          gitAndTools.git-octopus
          gitAndTools.git-crypt
          gitFull
          mercurial
          sshfsFuse
          sshuttle
          jq
          libreoffice
          (officeCommand "word" "WINWORD.EXE")
          (officeCommand "excel" "EXCEL.EXE")
          (officeCommand "powerpoint" "POWERPNT.EXE")
          httpie
          cloc
          mosh
          cheat
          graphicsmagick
          gdb
          gnupg1compat
          direnv
          ghostscript
          anbox
          skype
        ] ++ latex;
    };
    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };
  };
}
