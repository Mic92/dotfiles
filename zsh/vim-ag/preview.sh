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
syncat -t 'Solarized (light)' "${args[1]}" \
	| sed 's/253;//g' \
	| tail -n+$context \
	| ag --color --passthrough "${keyword}"
