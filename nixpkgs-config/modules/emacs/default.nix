{ pkgs, lib, config, ... }:

with lib;

let
  myemacs = config.programs.emacs.package;
  editorScript = {
    name ? "emacseditor",
    x11 ? false,
    extraArgs ? []
  }: pkgs.writeScriptBin name ''
      #!${pkgs.runtimeShell}
      export TERM=xterm-direct
      # breaks clipetty in combination with tmux + mosh? https://github.com/spudlyo/clipetty/pull/22
      unset SSH_TTY
      exec -a emacs ${myemacs}/bin/emacsclient \
        --create-frame \
        --alternate-editor ${myemacs}/bin/emacs \
        ${optionalString (!x11) "-nw"} \
        ${toString extraArgs} "$@"
    '';

  daemonScript = pkgs.writeScript "emacs-daemon" ''
    #!${pkgs.zsh}/bin/zsh
    source ~/.zshrc
    export BW_SESSION=1 PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    exec ${myemacs}/bin/emacs --daemon
  '';

  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };
in {
  home.file.".emacs.d/init.el".text = lib.mkBefore ''
    (load "${pkgs.fetchFromGitHub {
      owner = "seanfarley";
      repo = "emacs-bitwarden";
      rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
      sha256 = "sha256-ooLgOwpJX9dgkWEev9xmPyDVPRx4ycyZQm+bggKAfa0=";
    }}/bitwarden.el")
  '';

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../../../home/.doom.d;
    extraPackages = [ pkgs.mu ];
  };

  home.packages = with pkgs; [
    (lib.hiPrio editorScriptX11)
    ripgrep
    (lib.hiPrio (makeDesktopItem {
      name = "emacs";
      desktopName = "Emacs (Client)";
      exec = "${editorScriptX11}/bin/emacs %F";
      icon = "emacs";
      genericName = "Text Editor";
      comment = "Edit text";
      categories = "Development;TextEditor;Utility";
      mimeType = "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++";
    }))

    (lib.hiPrio (makeDesktopItem {
      name = "emacs-mailto";
      desktopName = "Emacs (MailTo)";
      # %u contains single quotes
      exec = ''${editorScriptX11}/bin/emacs --eval "(browse-url (replace-regexp-in-string \"'\" \"\" \"%u\"))"'';
      icon = "emacs";
      genericName = "Text Editor";
      comment = "Send email with Emacs";
      categories = "Utility";
      mimeType = "x-scheme-handler/mailto";
    }))

    (editorScript {
      name = "mu4e";
      x11 = true;
      extraArgs = [ "--eval" "'(mu4e)'" ];
    })
    (editorScript {})
    gopls
    golangci-lint
    (pkgs.runCommand "gotools-${gotools.version}" {} ''
      mkdir -p $out/bin
      # skip tools colliding with binutils
      for p in ${gotools}/bin/*; do
        name=$(basename $p)
        if [[ -x "${binutils-unwrapped}/bin/$name" ]]; then
          continue
        fi
        ln -s $p $out/bin/
      done
    '')
    gotests
    reftools
    gomodifytags
    gopkgs
    impl
    godef
    gogetdoc
  ];

  systemd.user.services.emacs-daemon = {
    Install.WantedBy = [ "default.target" ];
    Service = {
      Type = "forking";
      TimeoutStartSec = "10min";
      # bitwarden.el checks if that value is set,
      # we set it in our own bw wrapper internally
      Restart = "always";
      ExecStart = toString daemonScript;
    };
  };
}
