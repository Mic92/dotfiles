
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.programs.emacs;

  sourcesJson = builtins.fromJSON (builtins.readFile ../../../nixos/nix/sources.json);
  editorScript = { name ? "emacseditor", x11 ? false } : pkgs.writeScriptBin name ''
    #!${pkgs.runtimeShell}
    export TERM=xterm-direct
    exec -a emacs ${cfg.package}/bin/emacsclient \
      --socket-name $XDG_RUNTIME_DIR/emacs \
      --create-frame \
      --alternate-editor ${cfg.package}/bin/emacs \
      ${optionalString (!x11) "-nw"} \
      "$@"
  '';
  daemonScript = pkgs.writeScript "emacs-daemon" ''
    #!${pkgs.zsh}/bin/zsh
    source ~/.zshrc
    export BW_SESSION=1 PATH=$PATH:${pkgs.sqlite}/bin:${pkgs.git}/bin

    cd $HOME/.emacs.d/
    if [ ! -d $HOME/.emacs.d/.git ]; then
      git init
      git remote add origin https://github.com/syl20bnr/spacemacs
      git pull origin develop
    fi
    hash=${sourcesJson.spacemacs.rev}
    if [[ ! "$(git log --format=%H -1)" = "$hash" ]]; then
      git fetch origin
      git reset --hard "$hash" >&2
    fi
    pushd
    exec ${cfg.package}/bin/emacs --daemon
  '';
  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };
  core = pkgs.emacsPackagesNgGen (pkgs.emacs.override {
    imagemagick = if cfg.imagemagick.enable then pkgs.imagemagick else null;
  });
  #spacemacs = pkgs.callPackage ./spacemacs-with-packages.nix {
  #  emacsWithPackages = core.emacsWithPackages;
  #} {
  #  layers = import ./spacemacs-layers.nix;
  #  themes = ps: [ ps.solarized-theme pkgs.mu core.emacs ];
  #};
  myemacs = (pkgs.emacsPackagesNgGen (pkgs.emacs.override {
        imagemagick = if cfg.imagemagick.enable then pkgs.imagemagick else null;
  })).emacsWithPackages (ps: [pkgs.mu]);
in {
  options.programs.emacs = {
    socket-activation.enable =
      (mkEnableOption "socket-activation") // { default = pkgs.stdenv.isLinux; };
    imagemagick.enable = mkEnableOption "imagemagick";
  };
  config = mkMerge [
    ({
      programs.emacs.package = myemacs;
      #programs.emacs.package = spacemacs;
      home.packages = with pkgs; [
        editorScriptX11
        (makeDesktopItem {
          name = "emacs";
          desktopName = "Emacs (Client)";
          exec = "${editorScriptX11}/bin/emacs %F";
          icon = "emacs";
          genericName = "Text Editor";
          comment = "Edit text";
          categories = "Development;TextEditor;Utility";
          mimeType = "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++";
        })

        (pkgs.writeScriptBin "mu4e" ''
          #!${pkgs.runtimeShell}
          export BW_SESSION=1
          exec -a emacs ${cfg.package}/bin/emacs -e '(mu4e)'
        '')
        (editorScript {})
        gopls
        golangci-lint
        gotools
        gotests
        reftools
        gomodifytags
        gopkgs
        impl
        godef
        gogetdoc
      ];
    })
    (mkIf config.programs.emacs.socket-activation.enable {
      systemd.user.sockets.emacs-daemon = {
        Socket.ListenStream = "%t/emacs";
        Install.WantedBy = [ "sockets.target" ];
      };

      systemd.user.services.emacs-daemon = {
        Unit = {
          Requires = [ "emacs-daemon.socket" ];
          RefuseManualStart = true;
        };
        Service = {
          Type = "forking";
          TimeoutStartSec = "10min";
          # bitwarden.el checks if that value is set,
          # we set it in our own bw wrapper internally
          Restart = "always";
          ExecStart = toString daemonScript;
        };
      };
    })
  ];
}
