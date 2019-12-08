
{ pkgs, lib, config, ... }:

let
  myEmacs = ((pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages (epkgs: [ pkgs.mu ]));
  editorScript = { x11 ? false } : pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    export TERM=xterm-24bit
    exec -a emacs ${myEmacs}/bin/emacsclient \
      --socket-name $XDG_RUNTIME_DIR/emacs \
      --create-frame \
      --alternate-editor ${myEmacs}/bin/emacs \
      ${lib.optionalString x11 "-nw"} "$@"
  '';
  editorScriptX11 = editorScript { x11 = true; };

in {
  options = {
    programs.emacs.socket-activation.enable = (lib.mkEnableOption "socket-activation") // {
      default = true;
    };
  };
  config = lib.mkMerge [

    ({
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

        (editorScript {})
        gocode
        godef
        gocode
        go-tools
        gogetdoc
        impl
        gometalinter
        nur.repos.mic92.xterm-24bit-terminfo
      ];
    })
    (lib.mkIf config.programs.emacs.socket-activation.enable {
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
          ExecStart = "${pkgs.zsh}/bin/zsh -c 'source ~/.zshrc; export PATH=$PATH:${pkgs.sqlite}/bin; exec ${myEmacs}/bin/emacs --daemon'";
          Restart = "always";
        };
      };
    })
  ];
}
