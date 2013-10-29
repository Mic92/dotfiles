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
  systemctl --user set-environment GPG_TTY=$GPG_TTY
  systemctl --user set-environment GPG_AGENT_INFO=$GPG_AGENT_INFO
fi

if [[ $TTY == "/dev/tty1" ]]; then
  exec startx -- vt1
fi
