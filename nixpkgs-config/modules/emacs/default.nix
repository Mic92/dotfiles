
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.programs.emacs;

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
    export BW_SESSION=1 PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    exec ${cfg.package}/bin/emacs --daemon
  '';
  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };

  emacsOverlay = import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  }) pkgs pkgs;

  myemacs = (pkgs.emacsPackagesNgGen (emacsOverlay.emacsGit.override {
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
      home.file = let
        spacemacs = (import ../../../nixos/nix/sources.nix).spacemacs;
        spacemacsDirs = builtins.readDir spacemacs;
        mapDir = name: _: nameValuePair ".emacs.d/${name}" {
          recursive = name == "private";
          source = "${spacemacs}/${name}";
        };
      in mapAttrs' mapDir spacemacsDirs;

      programs.emacs.package = myemacs;
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
