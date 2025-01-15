function prompt_git_info
  set -l git (env LC_ALL=C git status --ignore-submodules ^/dev/null)
  if test $status = 0
    set -l red (set_color -o red)
    set -l yellow (set_color -o yellow)
    echo $git | awk 'BEGIN { branch = "no-branch"; branch_status=""; dirty="⚡" }
/On branch/ { branch = $3; }
/# On branch/ { branch = $4; }
/nothing to commit/ {dirty = ""}
/branch is ahead/    {branch_status = "↑"}
/branch is diverged/ {branch_status = "↕"}
/branch is behind/   {branch_status = "↓"}
END {
 print branch
 print branch_status
 print dirty
}
'
  end
end

set -g _prompt_old_print_pwd $PWD
set -g _prompt_ruby_string
set -g _prompt_git_info

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color -o cyan)
  set -l red (set_color -o red)
  set -l blue (set_color -o blue)
  set -l normal (set_color normal)
  set -l yellow (set_color -o yellow)

  set -l return_status ""
  if test $last_status != 0
    set return_status "$yellow($red$last_status$yellow)"
  end
  set -l arrow "$red➜ $normal"
  set -l cwd $cyan(prompt_pwd)

  set -l git_info
  if test "$_prompt_git_info" != "" -o -d .git -o -d ../.git
    set _prompt_git_info (prompt_git_info)
    if test (count $_prompt_git_info) = 3
      set -l git_branch $red$_prompt_git_info[1]
      set git_info "$blue on $git_branch$blue"
      if test -n $_prompt_git_info[2]
        set git_info "$git_info$red $_prompt_git_info[2]"
      end
      if test -n $_prompt_git_info[3]
        set git_info "$git_info$yellow ✗"
      end
    end
  end

  contains fry-current (functions); and set _prompt_ruby_string (fry-current)
  set -l ruby_version "$yellow$_prompt_ruby_string"
  set -l ruby_version "$ruby_version$normal in "

  echo -s $ruby_version $cwd $git_info $normal \n $return_status $arrow " "
end

function _print_duration --on-event fish_prompt
  set -l yellow (set_color -o yellow)
  if set -q CMD_DURATION
    echo  \a$yellow\> $CMD_DURATION
    set -l time_sec (echo $CMD_DURATION | awk 'BEGIN { FS="." } ; { print $1 }')
  end
end

set -g _old_pwd $PWD
function _ls_after_cd --on-event fish_prompt;
  if test "$_old_pwd" != "$PWD"
    set _old_pwd $PWD
    ls
  end
end
