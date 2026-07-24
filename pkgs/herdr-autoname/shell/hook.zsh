# herdr-autoname zsh hook: rename the current tab on preexec/precmd.
# Only active inside a herdr pane (HERDR_TAB_ID is set by herdr).

_herdr_autoname_bin="${${(%):-%N}:A:h:h}/bin/herdr-autoname"

if [[ -n ${HERDR_TAB_ID:-} && -x $_herdr_autoname_bin ]]; then
  _herdr_autoname_preexec() {
    # $2 is the alias-expanded command line; $1 is what was typed.
    ("$_herdr_autoname_bin" preexec "${2:-$1}" &) >/dev/null 2>&1
  }
  _herdr_autoname_precmd() {
    ("$_herdr_autoname_bin" precmd zsh &) >/dev/null 2>&1
  }
  autoload -Uz add-zsh-hook
  add-zsh-hook preexec _herdr_autoname_preexec
  add-zsh-hook precmd _herdr_autoname_precmd
fi
