{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  programs.xonsh = let
    extensions = pkgs.runCommand "foo" {} ''
      install -D ${./my_extensions.py} $out/my_extensions.py
    '';
  in {
    enable = true;
    package = self.packages.${pkgs.system}.xonsh;
    config = ''
      import sys
      sys.path.append("${extensions}")
      $PATH.add("${pkgs.fzf}/bin")
      $PATH.add("${pkgs.starship}/bin")

      $fzf_history_binding = "c-r"     # Ctrl+R
      $fzf_file_binding = "c-t"        # Ctrl+T
      $fzf_find_command = "fd"
      $fzf_find_dirs_command = "fd -t d"

      import my_extensions

      xontrib load direnv
      xontrib load prompt_starship
      xontrib load dracula
      xontrib load fzf_widgets

      aliases["vim"] = "emacseditor"
      aliases["tig"] = "lazygit"

      $XONSH_HISTORY_BACKEND = "sqlite"
      $HISTCONTROL = "ignoredups,ignoreerr"

      $XONSH_COLOR_STYLE = "dracula"
      $STARSHIP_CONFIG = "${(pkgs.formats.toml {}).generate "starship.toml" config.programs.starship.settings}"

      $LESS='-FXisRM'
      $LESS_TERMCAP_mb='\033[01;31m'     # begin blinking
      $LESS_TERMCAP_me='\033[0m'         # end mode
      $LESS_TERMCAP_se='\033[0m'         # end standout-mode
      $LESS_TERMCAP_so='\033[01;44;33m'  # begin standout-mode - info box
      $LESS_TERMCAP_ue='\033[0m'         # end underline
      $LESS_TERMCAP_us='\033[03;33;146m' # begin underline is now yellow, italic
      #                            |  |----------------- yellow
      #                            |-------------------- italic

      # we don't want to polute the PYTHONPATH with xonsh
      del $PYTHONPATH
    '';
  };
  programs.starship = {
    enable = true;
    settings = {
      git_status.disabled = true;
      hostname.ssh_only = false;
    };
  };
}
