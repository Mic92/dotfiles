set confirm off
set verbose off

#set prompt \033[31mgdb$ \033[0m
set print pretty on
set history save on
#set detach-on-fork off
set listsize 10

set disassembly-flavor intel

# These make gdb never pause in its output
set height 0
set width 0

set auto-load safe-path /
set breakpoint pending on
define gef
	source /home/joerg/.gdbinit-gef.py
end

define src
  layout src
end

define segfaultaddr
  p $_siginfo._sifields._sigfault.si_addr
end

macro define offsetof(t, f) &((t *) 0)->f
