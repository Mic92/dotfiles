{ pkgs, lib, config, ... }:

let
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    export TERM=alacritty
    exec ${pkgs.emacs}/bin/emacsclient \
      --socket-name $XDG_RUNTIME_DIR/emacs \
      --create-frame \
      --alternate-editor ${pkgs.emacs}/bin/emacs \
      -nw "$@"
  '';

in {
  options = {
    programs.emacs.socket-activation.enable = (lib.mkEnableOption "socket-activation") // {
      default = true;
    };
  };
  config = lib.mkMerge [
    ({
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
    })
    (lib.mkIf config.programs.emacs.socket-activation.enable {
      systemd.user.sockets.emacs-daemon = {
        Socket.ListenStream = "%t/emacs";
        Install.WantedBy = [ "sockets.target" ];
      };

      systemd.user.services.emacs-daemon = {
        Unit.RefuseManualStart = true;
        Service = {
          Type = "forking";
          ExecStart = "${pkgs.zsh}/bin/zsh -c 'source ~/.zshrc; exec ${pkgs.emacs}/bin/emacs --daemon'";
          ExecStop = "${pkgs.emacs}/bin/emacsclient -s %t/emacs --eval (kill-emacs)";
          Restart = "always";
        };
        Install.WantedBy = [ "default.target" ];
      };
    })
  ];
}
