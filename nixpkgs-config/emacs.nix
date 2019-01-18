{ pkgs, ... }:

let
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec ${pkgs.emacs}/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs --create-frame --alternate-editor ${pkgs.emacs}/bin/emacs
    else
      exec ${pkgs.emacs}/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs --alternate-editor ${pkgs.emacs}/bin/emacs "$@"
    fi
  '';

in {
  home.packages = with pkgs; [
    editorScript
    gocode
    godef
    gocode
    go-tools
    gogetdoc
    impl
    gometalinter
  ];

  systemd.user.sockets.emacs-daemon = {
    Socket.ListenStream = "%t/emacs";
    Install.WantedBy = [ "sockets.target" ];
  };

  systemd.user.services.emacs-daemon = {
    Unit.RefuseManualStart = true;
    Service = {
      Type = "forking";
      ExecStart = "${pkgs.zsh}/bin/zsh -c 'source ~/.zshrc; exec ${pkgs.emacs}/bin/emacs --daemon'";
      ExecStop = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
      Restart = "always";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
