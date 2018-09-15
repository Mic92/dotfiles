#!/usr/bin/env zsh

keyword=$1
shift

setopt shwordsplit
IFS=':' args=($@)
unsetopt shwordsplit
# sed removes ugly background color
context=`expr ${args[2]} - 10`
if (( $context < 0 )); then
	context=0
fi
bat --color=always --theme 'Monokai Extended Light' --line-range $context: "${args[1]}" \
	| ag --color --passthrough "${keyword}"
