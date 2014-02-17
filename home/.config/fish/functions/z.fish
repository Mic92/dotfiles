# maintains a jump-list of the directories you actually use
#
# INSTALL:
#   * put something like this in your config.fish:
#     . /path/to/z.fish
#   * put something like this in your fish_prompt function:
#       z --add "$PWD"
#   * cd around for a while to build up the db
#   * PROFIT!!
#
# USE:
#   * z foo     # goes to most frecent dir matching foo
#   * z foo bar # goes to most frecent dir matching foo and bar
#   * z -r foo  # goes to highest ranked dir matching foo
#   * z -t foo  # goes to most recently accessed dir matching foo
#   * z -l foo  # list all dirs matching foo (by frecency)

function z -d "Jump to a recent directory."
    set -l datafile "$HOME/.z"

    # add entries
    if [ "$argv[1]" = "--add" ]
        set -e argv[1]

        # $HOME isn't worth matching
        [ "$argv" = "$HOME" ]; and return

        set -l tempfile (command mktemp $datafile.XXXXXX)
        test -f $tempfile; or return

        # maintain the file
        command awk -v path="$argv" -v now=(date +%s) -F"|" '
            BEGIN {
                rank[path] = 1
                time[path] = now
            }
            $2 >= 1 {
                if( $1 == path ) {
                    rank[$1] = $2 + 1
                    time[$1] = now
                } else {
                    rank[$1] = $2
                    time[$1] = $3
                }
                count += $2
            }
            END {
                if( count > 1000 ) {
                    for( i in rank ) print i "|" 0.9*rank[i] "|" time[i] # aging
                } else for( i in rank ) print i "|" rank[i] "|" time[i]
            }
        ' $datafile ^/dev/null > $tempfile

        command mv -f $tempfile $datafile

    # tab completion
    else
        if [ "$argv[1]" = "--complete" ]
            command awk -v q="$argv[2]" -F"|" '
                BEGIN {
                    if( q == tolower(q) ) nocase = 1
                    split(q,fnd," ")
                }
                {
                    if( system("test -d \"" $1 "\"") ) next
                    if( nocase ) {
                        for( i in fnd ) tolower($1) !~ tolower(fnd[i]) && $1 = ""
                        if( $1 ) print $1
                    } else {
                        for( i in fnd ) $1 !~ fnd[i] && $1 = ""
                        if( $1 ) print $1
                    }
                }
            ' "$datafile" 2>/dev/null

        else
            # list/go
            set -l last ''
            set -l list 0
            set -l typ ''
            set -l fnd ''

            while [ (count $argv) -gt 0 ]
                switch "$argv[1]"
                    case -- '-h'
                        echo "z [-h][-l][-r][-t] args" >&2
                        return
                    case -- '-l'
                        set list 1
                    case -- '-r'
                        set typ "rank"
                    case -- '-t'
                        set typ "recent"
                    case -- '--'
                        while [ "$argv[1]" ]
                            set -e argv[1]
                            set fnd "$fnd $argv[1]"
                        end
                    case '*'
                        set fnd "$fnd $argv[1]"
                end
                set last $1
                set -e argv[1]
            end

            [ "$fnd" ]; or set list 1

            # if we hit enter on a completion just go there
            [ -d "$last" ]; and cd "$last"; and return

            # no file yet
            [ -f "$datafile" ]; or return

            set -l tempfile (command mktemp $datafile.XXXXXX)
            test -f $tempfile; or return
            set -l target (command awk -v t=(date +%s) -v list="$list" -v typ="$typ" -v q="$fnd" -v tmpfl="$tempfile" -F"|" '
                function frecent(rank, time) {
                    dx = t-time
                    if( dx < 3600 ) return rank*4
                    if( dx < 86400 ) return rank*2
                    if( dx < 604800 ) return rank/2
                    return rank/4
                }
                function output(files, toopen, override) {
                    if( list ) {
                        if( typ == "recent" ) {
                            cmd = "sort -nr >&2"
                        } else cmd = "sort -n >&2"
                        for( i in files ) if( files[i] ) printf "%-10s %s\n", files[i], i | cmd
                        if( override ) printf "%-10s %s\n", "common:", override > "/dev/stderr"
                    } else {
                        if( override ) toopen = override
                        print toopen
                    }
                }
                function common(matches, fnd, nc) {
                    for( i in matches ) {
                        if( matches[i] && (!short || length(i) < length(short)) ) short = i
                    }
                    if( short == "/" ) return
                    for( i in matches ) if( matches[i] && i !~ short ) x = 1
                    if( x ) return
                    if( nc ) {
                        for( i in fnd ) if( tolower(short) !~ tolower(fnd[i]) ) x = 1
                    } else for( i in fnd ) if( short !~ fnd[i] ) x = 1
                    if( !x ) return short
                }
                BEGIN { split(q, a, " ") }
                {
                    if( system("test -d \"" $1 "\"") ) next
                    print $0 >> tmpfl
                    if( typ == "rank" ) {
                        f = $2
                    } else if( typ == "recent" ) {
                        f = t-$3
                    } else f = frecent($2, $3)
                    wcase[$1] = nocase[$1] = f
                    for( i in a ) {
                        if( $1 !~ a[i] ) delete wcase[$1]
                        if( tolower($1) !~ tolower(a[i]) ) delete nocase[$1]
                    }
                    if( wcase[$1] > oldf ) {
                        cx = $1
                        oldf = wcase[$1]
                    } else if( nocase[$1] > noldf ) {
                        ncx = $1
                        noldf = nocase[$1]
                    }
                }
                END {
                    if( cx ) {
                        output(wcase, cx, common(wcase, a, 0))
                    } else if( ncx ) output(nocase, ncx, common(nocase, a, 1))
                }
            ' "$datafile")

            if [ $status -gt 0 ]
                command rm -f "$tempfile"
            else
                command mv -f "$tempfile" "$datafile"
                [ "$target" ]; and cd "$target"
            end
        end
    end
end
