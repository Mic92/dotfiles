function posix_source
  if set -q $1
    eval (sh -c "source $1; env" | grep -Ev '^(PWD|SHLVL|HOME|_)' |sed 's/^\([^=]\+\)=\(.*\)/set -x \1 \'\2\';/')
  end
end

