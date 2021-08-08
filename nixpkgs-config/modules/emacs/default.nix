{ pkgs, lib, config, ... }:

with lib;

let
  myemacs = pkgs.emacsGcc;
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
    if [ ! -d $HOME/.emacs.d/.git ]; then
      mkdir -p $HOME/.emacs.d
      git -C $HOME/.emacs.d init
      git -C $HOME/.emacs.d remote add origin git@github.com:hlissner/doom-emacs.git
    fi
    if [ $(git -C $HOME/.emacs.d rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.emacs.d fetch origin || true
      git -C $HOME/.emacs.d checkout ${pkgs.doomEmacsRevision} || true
      $HOME/.emacs.d/bin/doom sync || true
    fi
    exec ${myemacs}/bin/emacs --daemon
  '';

  editorScriptX11 = editorScript { name = "emacs"; x11 = true; };
in
{
  home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") pkgs.tree-sitter.builtGrammars)};
  '');

  home.packages = with pkgs; [
    myemacs
    ripgrep
    mu
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
