set -l FISH_HOME $HOME/.config/fish/

source $FISH_HOME/profile.fish

if test -f /usr/share/fry/fry.fish;
  source /usr/share/fry/fry.fish
end

if test -f "$HOME/.homesick/repos/homeshick/homeshick.fish"
  source "$HOME/.homesick/repos/homeshick/homeshick.fish"
end

if status --is-login
  posix_source --sh /etc/profile
  source $FISH_HOME/login.fish
end

if status --is-interactive
  source $FISH_HOME/rc.fish
  source $FISH_HOME/solarized.fish
end
