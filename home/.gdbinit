set confirm off
set verbose off
#set prompt \033[31mgdb$ \033[0m
set print pretty on
set history save on
#set detach-on-fork off
set listsize 10

add-auto-load-safe-path /usr/lib/go/src/runtime/runtime-gdb.py

set auto-load safe-path /
set breakpoint pending on
define gef
	source /home/joerg/.gdbinit-gef.py
end

