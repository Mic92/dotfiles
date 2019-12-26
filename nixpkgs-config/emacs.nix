
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.programs.emacs;
  editorScript = { name ? "emacseditor", x11 ? false, mu4e ? false } : pkgs.writeScriptBin name ''
    #!${pkgs.runtimeShell}
    export TERM=xterm-24bit
    exec -a emacs ${cfg.package}/bin/emacsclient \
      --socket-name $XDG_RUNTIME_DIR/emacs \
      --create-frame \
      --alternate-editor ${cfg.package}/bin/emacs \
      ${optionalString (!x11) "-nw"} \
      ${optionalString (mu4e) "-e '(mu4e)'"} \
      "$@"
  '';
  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };

in {
  options.programs.emacs = {
    socket-activation.enable =
      (mkEnableOption "socket-activation") // { default = true; };

    extraPackages = mkOption {
      default = ps: [];
      defaultText = "ps: []";
      type = types.listOf types.package;
      example = "(ps: [ pkgs.mu ])";
    };
  };
  config = mkMerge [
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

        (editorScript { name = "mu4e"; mu4e = true; x11 = true; })
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
          # bitwarden.el checks if that value is set,
          # we set it in our own bw wrapper internally
          ExecStart = "${pkgs.zsh}/bin/zsh -c 'source ~/.zshrc; export BW_SESSION=1 PATH=$PATH:${pkgs.sqlite}/bin; exec ${cfg.package}/bin/emacs --daemon'";
          Restart = "always";
        };
      };
    })
  ];
}
