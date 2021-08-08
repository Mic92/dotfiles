{ pkgs, lib, config, ... }:

with lib;

let
  myemacs = config.programs.emacs.package;
  editorScript =
    { name ? "emacseditor"
    , x11 ? false
    , extraArgs ? [ ]
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

  treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)};
  '';

  daemonScript = pkgs.writeScript "emacs-daemon" ''
    #!${pkgs.zsh}/bin/zsh
    source ~/.zshrc
    export PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    if [ ! -d $HOME/.emacs ]; then
      git clone git@github.com:hlissner/doom-emacs.git $HOME/.emacs
    fi
    git -C $HOME/.emacs pull || true
    git -C $HOME/.emacs checkout ${pkgs.doomEmacsRevision} || true
    $HOME/.emacs/bin/doom sync
    exec ${myemacs}/bin/emacs --daemon
  '';

  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };
in
{
  programs.emacs.package = pkgs.emacsGcc;

  home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") pkgs.tree-sitter.builtGrammars)};
  '');

  home.packages = with pkgs; [
    (lib.hiPrio editorScriptX11)
    ripgrep
    mu
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
    (editorScript { })
    gopls
    golangci-lint
    (pkgs.runCommand "gotools-${gotools.version}" { } ''
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
      Restart = "always";
      ExecStart = toString daemonScript;
    };
  };
}
