local _plugin__ssh_env=$HOME/.ssh/environment-$HOST
local _plugin__forwarding

function _plugin__start_agent() {
  # start ssh-agent and setup environment
  /usr/bin/env ssh-agent | sed 's/^echo/#echo/' > ${_plugin__ssh_env}
  chmod 600 ${_plugin__ssh_env}
  . ${_plugin__ssh_env} > /dev/null

  # load identies
  echo starting ssh-agent...
  ssh-add $HOME/.ssh/id_(ecdsa|rsa)
}

if [[ -n "$SSH_AUTH_SOCK" ]]; then
  # Add a nifty symlink for screen/tmux if agent forwarding
  [[ -L $SSH_AUTH_SOCK ]] || ln -sf "$SSH_AUTH_SOCK" /tmp/ssh-agent-$USER-screen
elif [ -f "${_plugin__ssh_env}" ]; then
  # Source SSH settings, if applicable
  . ${_plugin__ssh_env} >/dev/null
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ >/dev/null || _plugin__start_agent
else
  _plugin__start_agent;
fi

# tidy up after ourselves
unfunction _plugin__start_agent
unset _plugin__forwarding
unset _plugin__ssh_env

local GPG_ENV=$HOME/.gnupg/gpg-agent.env

# check if another agent is running
if ! gpg-connect-agent --quiet /bye > /dev/null 2> /dev/null; then
    # source settings of old agent, if applicable
    if [ -f "${GPG_ENV}" ]; then
      . ${GPG_ENV} > /dev/null
    fi

    # check again if another agent is running using the newly sourced settings
    if ! gpg-connect-agent --quiet /bye > /dev/null 2> /dev/null; then
      eval $(/usr/bin/env gpg-agent --quiet --daemon --write-env-file ${GPG_ENV} 2> /dev/null)
      chmod 600 ${GPG_ENV}
      export GPG_AGENT_INFO
    fi
fi

GPG_TTY=$(tty)
export GPG_TTY

if [ -n ${commands[systemctl]} ]; then
  systemctl --user set-environment SSH_AUTH_SOCK=$SSH_AUTH_SOCK
  systemctl --user set-environment SSH_AGENT_PID=$SSH_AGENT_PID
  systemctl --user set-environment GPG_TTY=$GPG_TTY
  systemctl --user set-environment GPG_AGENT_INFO=$GPG_AGENT_INFO
fi

if [[ $TTY == "/dev/tty1" ]]; then
  exec startx -- vt1
fi
