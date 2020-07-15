
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
  editorScriptX11 = editorScript { name = "emacs-x11"; x11 = true; };

  myemacs = pkgs.callPackage (builtins.fetchTarball {
    url = "https://github.com/vlaci/nix-doom-emacs/archive/develop.tar.gz";
  }) {
    doomPrivateDir = ../../../home/.doom.d;
    extraPackages = [ pkgs.mu ];
  };
in {
  options.programs.emacs = {
    socket-activation.enable =
      (mkEnableOption "socket-activation") // { default = pkgs.stdenv.isLinux; };
    imagemagick.enable = mkEnableOption "imagemagick";
  };
  config = mkMerge [
    ({
      home.file.".emacs.d/init.el".text = ''
        (load "${pkgs.fetchFromGitHub {
          owner = "Mic92";
          repo = "osc52";
          rev = "89f1bafd096efca45c8e02dfc4e4fbd76afb1c98";
          sha256 = "sha256-qThYQKi2xHDvhB4a6lGMz4VYXEc3gr+PAHqzD+wRPoc=";
        }}/osc52.el")
        (load "${pkgs.fetchFromGitHub {
          owner = "seanfarley";
          repo = "emacs-bitwarden";
          rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
          sha256 = "sha256-ooLgOwpJX9dgkWEev9xmPyDVPRx4ycyZQm+bggKAfa0=";
        }}/bitwarden.el")
        (load "default.el")
      '';

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
        (pkgs.lowPrio myemacs)
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
