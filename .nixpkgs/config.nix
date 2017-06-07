with import <nixpkgs> {};
let
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
    vimrcConfig.customRC = ''
      if filereadable($HOME . "/.vimrc")
        source ~/.vimrc
      endif
    '';
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
        ack-vim
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

  rubyApps = pkgs.loadRubyEnv ./.bundix/definition.nix {
    paths = with pkgs; [ bundler bundix rubocop ];
  };

  desktopApps = [
    dino
    libreoffice
    (officeCommand "word" "WINWORD.EXE")
    (officeCommand "excel" "EXCEL.EXE")
    (officeCommand "powerpoint" "POWERPNT.EXE")
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


  rustApps = [
    rustc
    cargo
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

  nixDevApps = [
    nix-prefetch-scripts
    pypi2nix
    go2nix
    mercurial # go2nix
    bundix
    #nox
    nix-repl
  ];

  debuggingBasicsApps = [
    gdb
    strace
  ];
  userPackages = name: paths: buildEnv {
    inherit ((import <nixpkgs/nixos> {}).config.system.path)
      pathsToLink ignoreCollisions postBuild;
    extraOutputsToInstall = [ "man" ];
    inherit paths name;
  };
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
    all = userPackages "all" ([]
      #++ desktopApps
      #++ latexApps
      #++ rustApps
      #++ rubyApps
      #++ pythonDataLibs
      ++ nixDevApps
      ++ debuggingApps
      ++ [
        vim
        gitAndTools.diff-so-fancy
        gitAndTools.hub
        gitAndTools.git-octopus
        gitAndTools.git-crypt
        gitFull
        sshfsFuse
        sshuttle
        jq
        httpie
        cloc
        mosh
        cheat
        graphicsmagick
        gnupg1compat
        direnv
        ghostscript
        tree
      ]);
    
    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };

    debuggingApps = [
      gperftools
      valgrind
      binutils
    ];

    pythonDataLibs = with python3Packages; [
      ipython
      numpy
      scipy
      matplotlib
      pandas
      seaborn
    ];
  };
}
