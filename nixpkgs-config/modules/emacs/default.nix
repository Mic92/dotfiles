
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.programs.emacs;

  editorScript = {
    name ? "emacseditor",
    x11 ? false,
    extraArgs ? []
  }: pkgs.writeScriptBin name ''
      #!${pkgs.runtimeShell}
      export TERM=xterm-direct
      exec -a emacs ${cfg.package}/bin/emacsclient \
        --socket-name $XDG_RUNTIME_DIR/emacs \
        --create-frame \
        --alternate-editor ${cfg.package}/bin/emacs \
        ${optionalString (!x11) "-nw"} \
        ${toString extraArgs} "$@"
    '';
  daemonScript = pkgs.writeScript "emacs-daemon" ''
    #!${pkgs.zsh}/bin/zsh
    source ~/.zshrc
    export BW_SESSION=1 PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    exec ${cfg.package}/bin/emacs --daemon
  '';
  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };

  emacs-direnv = pkgs.writeScriptBin "emacs-direnv" ''
    #!${pkgs.runtimeShell}
    timeout 1 ${pkgs.direnv}/bin/direnv "$@"
    res=$!
    if [[ "$res" == 124 ]]; then
      echo "{}" >&2
      exit 0
    fi
    exit $res
  '';

  flake = (builtins.fromJSON (builtins.readFile ../../../flake.lock)).nodes;
  tarballUrl = info: "https://api.github.com/repos/${info.owner}/${info.repo}/tarball/${info.rev}";
  nix-doom-emacs = builtins.fetchTarball (tarballUrl flake.nix-doom-emacs.locked);
  doom-emacs = builtins.fetchTarball (tarballUrl flake.doom-emacs.locked);

  #emacsOverlay = (import (builtins.fetchTarball {
  #    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #  })) pkgs pkgs;

  myemacs = pkgs.callPackage nix-doom-emacs {
  # For testing
  #myemacs = pkgs.callPackage /home/joerg/git/nix-doom-emacs {
    doomPrivateDir = builtins.path {
      name = "doom.d";
      path = ../../../home/.doom.d;
    };
    dependencyOverrides.doom-emacs = doom-emacs;
    extraPackages = [ pkgs.mu ];
    # maybe some day, when it stops segfaulting
    #emacsPackages = emacsOverlay.emacsPackagesFor emacsOverlay.emacsGcc;
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
          owner = "seanfarley";
          repo = "emacs-bitwarden";
          rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
          sha256 = "sha256-ooLgOwpJX9dgkWEev9xmPyDVPRx4ycyZQm+bggKAfa0=";
        }}/bitwarden.el")
        (load "default.el")
      '';

      programs.emacs.package = myemacs;
      home.packages = with pkgs; [
        emacs-all-the-icons-fonts
        emacs-direnv
        editorScriptX11
        ripgrep
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

        (makeDesktopItem {
          name = "emacs-mailto";
          desktopName = "Emacs (MailTo)";
          # %u contains single quotes
          exec = ''${editorScriptX11}/bin/emacs --eval "(browse-url (replace-regexp-in-string \"'\" \"\" \"%u\"))"'';
          icon = "emacs";
          genericName = "Text Editor";
          comment = "Send email with Emacs";
          categories = "Utility";
          mimeType = "x-scheme-handler/mailto";
        })

        (editorScript {
          name = "mu4e";
          x11 = true;
          extraArgs = [ "--eval" "'(mu4e)'" ];
        })
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
