"===================================================================================
" GENERAL SETTINGS
"===================================================================================
"
"-------------------------------------------------------------------------------
" Use Vim settings, rather then Vi settings.
" This must be first, because it changes other options as a side effect.
"-------------------------------------------------------------------------------
set nocompatible
let mapleader=","             " change the leader to be a comma vs slash
"
" Toggle the tasklist
map <leader>td <Plug>TaskList

filetype off
"call pathogen#runtime_append_all_bundles()
"call pathogen#helptags()
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" let Vundle manage Vundle
" required!
Bundle 'ack.vim'
Bundle 'Command-T'
Bundle 'Raimondi/delimitMate'
Bundle 'fakeclip'
Bundle 'vim-scripts/Gundo'
Bundle 'pep8'
Bundle 'TaskList.vim'
Bundle 'The-NERD-tree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'vimlatex'
Bundle 'vundle'
Bundle 'vim-scripts/habiLight'
Bundle 'ap/vim-css-color'
Bundle 'bronson/vim-trailing-whitespace'
Bundle 'tpope/vim-surround'
Bundle 'altercation/vim-colors-solarized'
Bundle 'kchmck/vim-coffee-script'
Bundle 'bbommarito/vim-slim'
Bundle 'tpope/vim-rails'
Bundle 'msanders/snipmate.vim'
Bundle 'alfredodeza/jacinto.vim'
Bundle 'davidhalter/jedi-vim'

syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin indent on     " enable loading indent file for filetype

" Enable 256 colors
set t_Co=256
"colorscheme zenburn
let g:solarized_termcolors=256
set background=light
"set background=dark
colorscheme solarized
" Platform specific items:
" - central backup directory (has to be created)
" - default dictionary
" Uncomment your choice.
if  has("win16") || has("win32") || has("win64") || has("win95")
"
"  runtime mswin.vim
"  set backupdir =$VIM\vimfiles\backupdir
"  set dictionary=$VIM\vimfiles\wordlists/german.list
else
  set backupdir =$HOME/.vim.backupdir
  set dictionary=$HOME/.vim/wordlists/german.list,$HOME/.vim/wordlists/english.list
endif
"
" spellchecker for latex
":setlocal spell spelllang=de
"
set grepprg=ack         " replace the default grep program with ack
nmap <leader>a <Esc>:Ack!
"
"-------------------------------------------------------------------------------
" Various settings
"------------------------------------------------------------------------------
set autoindent                       " copy indent from current line
set autoread                         " read open files again when changed outside Vim
set autowrite                        " write a modified buffer on each :next , ...
set backspace=indent,eol,start       " backspacing over everything in insert mode
set backup                           " keep a backup file
set browsedir=current                " which directory to use for the file browser
set complete+=k                      " scan the files given with the 'dictionary' option
set complete+=.,t
set completeopt=menuone,menu,longest " better completion
set iskeyword+=_,-                   " these characters also belong to a word
set expandtab
set foldmethod=marker                " use always folding marker
set gdefault                         " use g-modifier in regex by default
set history=50                       " keep 50 lines of command line history
set hlsearch                         " highlightthe last used search pattern
set hidden                           "
set incsearch                        " do incremental searching
set ignorecase                       " Ignore case when searching
set list                             " show whitespace
set listchars=tab:\ \ ,trail:·,eol:\  " strings to use in 'list' mode
set magic                            " Set magic on, for regular expressions
set modeline                         " React on modlines in files
set mouse=a                          " enable the use of the mouse
set nowrap                           " do not wrap lines
set noerrorbells                     " disable error bells
set novisualbell                     " disable beep
set popt=left:8pc,right:3pc          " print options
set smartindent
set showcmd                          " display incomplete commands
set showmatch                        " Show matching bracets when text indicator is over them
set smartindent                      " smart autoindenting when starting a new line
set smartcase
set tabstop=4
set shiftwidth=2
set textwidth=80
set shell=/bin/zsh
set title
set ttyfast                          " send more characters to screen for redrawing
set ttymouse=xterm2
set undofile                         " persistent undo
set undoreload=10000
set undodir=~/.vimundo
set ruler                            " show the cursor position all the time
"set tabstop=2                        " number of spaces that a <Tab> counts for
set visualbell                       " visual bell instead of beeping
set wrap                             " Wrap lines
set wildignore=*.bak,*.o,*.e,*~      " wildmenu: ignore these extensions
set wildmode=list:longest            " command-line completion in an enhanced mode
set omnifunc=syntaxcomplete#Complete " autocompletion for some languages
"
"-------------------------------------------------------------------------------
" comma always followed by a space
"-------------------------------------------------------------------------------
inoremap  ,  ,<Space>
" go in normal mode with jj
inoremap jj <Esc>
"
autocmd BufEnter * :lchdir %:p:h
"
" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
"
" Remove trailing whitespace on saving
"autocmd BufWritePre * :%s/\s\+$//e
" sudo to write
cmap w!! w !sudo tee % >/dev/null
"===================================================================================
" VARIOUS PLUGIN CONFIGURATIONS
"===================================================================================
"
" File Encoding
"------------------------------------------------------------------------------
" New files are created in utf-8.
set encoding=utf-8
"
" If the encoding of the file cannot be determined uniquely among those three, utf-8 is chosen.
setglobal fileencoding=utf-8
"
" Existing files are treated as utf-8-bom, utf-8 and latin-1.
set fileencodings=ucs-bom,utf-8,latin1
"
" If the encoding is none of those three, then the file is interpreted as latin-1.
set termencoding=utf-8
"
"------------------------------------------------------------------------------
" Haskell setup
"------------------------------------------------------------------------------
"
au Bufenter *.hs compiler ghc
let g:haddock_browser = "firefox"
"
"------------------------------------------------------------------------------
" OmniComplete
"------------------------------------------------------------------------------
"
" most language I use are case sensitive, so omnicomplete have to be too!
"let g:omni_syntax_ignorecase = 0
"
let g:clang_complete_copen = 1
"let g:clang_periodic_quickfix  = 1
let g:clang_snippets = 1
let g:clang_use_library = 1
let g:clang_user_options = '-I/usr/lib/clang/3.0/include'

"------------------------------------------------------------------------------
" LateX settings
"------------------------------------------------------------------------------
"
" Treat .tex-file as latex instead of plain-tex
let g:tex_flavor='latex'
"
" Determine the output format.
let g:Tex_DefaultTargetFormat='pdf'
"
" Additionally, if one wants the \lv command calling xdvi with this option, one inserts the following line to the .vimrc file
let g:Tex_ViewRuleComplete_pdf = 'evince'
"
" clean warnings
let g:Tex_IgnoredWarnings =
                        \'Underfull'."\n".
                        \'Overfull'."\n".
                        \'specifier changed to'."\n".
                        \'You have requested'."\n".
                        \'Missing number, treated as zero.'."\n".
                        \'There were undefined references'."\n".
                        \'Latex Warning:'."\n".
                        \'Citation %.%# undefined'
let g:Tex_IgnoreLevel = 8

"open zipped files
au BufReadCmd *.jar,*.xpi,*.wgt call zip#Browse(expand("<amatch>"))
runtime ftplugin/man.vim

" SuperTab
let g:SuperTabDefaultCompletionType = "context"

" Copy and paste with fakeclip
"Command-C and Command-V are also available in MacVim
" see :help fakeclip-multibyte-on-mac
map <leader>y "*y
map <leader>p "*p
if exists('$WINDOW') || exists('$TMUX')
        map <leader>Y (fakeclip-screen-y)
        map <leader>P (fakeclip-screen-p)
endif
"--------------------------------------------------------------------------------
" Vala settings
"--------------------------------------------------------------------------------
au BufEnter *.vala set ai sw=4 ts=4 sta et fo=croql
au BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala            setfiletype vala
au BufRead,BufNewFile *.vapi            setfiletype vala

"-------------------------------------------------------------------------------
" Python setup
"-------------------------------------------------------------------------------
au BufEnter *.py set ai sw=4 ts=4 sta et fo=croql
"call pathogen#infect('python')
let g:pep8_map='<leader>8'
map <leader>r :RopeRename<CR>
map <leader>j :RopeGotoDefinition<CR>
" Disable pylint checking every save
let g:pymode_lint_write = 0
let g:pymode_rope_always_show_complete_menu = 1
let g:pymode_rope_guess_project = 0
" faster alternative to pylint
let g:pymode_lint_checker = "pyflakes"
"
"au BufNewFile,BufRead,BufEnter *.cpp,*.hpp set omnifunc=omni#cpp#complete#Main
autocmd FileType cpp set omnifunc=omni#cpp#complete#Main
"
" Keybinding
"
" runs make
"
nmap <F4> :w<CR>:make<CR>:cw<CR>
"
" Android Hacking
au BufRead,BufNewFile *.smali set filetype=smali

" Mutt - limit the width of text to 72 characters
au BufRead /tmp/mutt-* set tw=72

set spell
