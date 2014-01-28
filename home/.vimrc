" Leader
let mapleader = " "

set nocompatible " Use Vim settings, rather then Vi settings
set ruler " show the cursor position all the time
set showcmd " display incomplete commands
set incsearch " do incremental searching
set laststatus=2 " Always display the status line
set smartcase
set ignorecase
set showmatch     " Show matching bracets when text indicator is over them
set autowrite     " write a modified buffer on each :next ,  ...

set modeline      " React on modlines in files
set hlsearch      " highlightthe last used search pattern

set ttyfast          " send more characters to screen for redrawing
set ttymouse=xterm2
set wildignore=*.bak,*.o,*.e,*~      " wildmenu: ignore these extensions
set iskeyword+=_,-  " these characters also belong to a word
set wrap                             " do wrap lines
set noerrorbells                     " disable error bells
set novisualbell                     " disable beep
set backspace=indent,eol,start       " backspacing over everything in insert mode
set browsedir=current                " which directory to use for the file browser
set shell=bash

" Declare bundles are handled via Vundle
filetype off " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Let Vundle manage Vundle
Bundle 'gmarik/vundle'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'scrooloose/nerdcommenter'
Bundle 'Raimondi/delimitMate'
Bundle 'fakeclip'
Bundle 'The-NERD-tree'
Bundle 'bronson/vim-trailing-whitespace'
Bundle 'altercation/vim-colors-solarized'
Bundle 'alfredodeza/jacinto.vim'
Bundle 'msanders/snipmate.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'slim-template/vim-slim'
Bundle 'ap/vim-css-color'
Bundle 'scrooloose/syntastic'
Bundle 'Valloric/YouCompleteMe'
Bundle 'airblade/vim-gitgutter'
Bundle 'SirVer/ultisnips'
Bundle 'kien/ctrlp.vim'
Bundle 'majutsushi/tagbar'
Bundle 'freitass/todo.txt-vim'
Bundle 'stefanoverna/vim-i18n'
Bundle 'dag/vim-fish'
"Bundle 'dhruvasagar/vim-table-mode'
"Bundle 'derekwyatt/vim-scala'
"Bundle 'wavded/vim-stylus'
"Bundle 'digitaltoad/vim-jade'

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
  syntax on

  " Enable 256 colors
  set t_Co=256
  "colorscheme zenburn
  let g:solarized_termcolors=256
  set background=light
  "set background=dark
  colorscheme solarized
endif

filetype plugin indent on

runtime macros/matchit.vim
Bundle 'kana/vim-textobj-user'
Bundle 'nelstrom/vim-textobj-rubyblock'

set textwidth=80

augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  "   autocmd FileType text setlocal textwidth=78  au!
  "
  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
augroup END

" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab

" Display extra whitespace
set list listchars=tab:»·,trail:·

map <C-n> :NERDTreeToggle<CR>

" Use Ag (https://github.com/ggreer/the_silver_searcher) instead of Grep when
" available
if executable('ag')
" Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

endif
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_dont_split = 'NERD_tree_2'
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
set complete=.,w,t
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

" Exclude Javascript files in :Rtags via rails.vim due to warnings when parsing
let g:Tlist_Ctags_Cmd="ctags --exclude='*.js'"

" Index ctags from any project, including those outside Rails
map <Leader>ct :!ctags -R .<CR>

" Switch between the last two files
nnoremap <leader><leader> <c-^>

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'

" Markdown files end in .md
au BufRead,BufNewFile *.md set filetype=markdown

" python setup
au BufEnter *.py set ai sw=4 ts=4 sta et fo=croql

" rust setup
au BufEnter *.rs set ai sw=4 ts=4 sta et fo=croql

" Copy and paste with fakeclip
"Command-C and Command-V are also available in MacVim
" see :help fakeclip-multibyte-on-mac
map <leader>y "*y
map <leader>p "*p
if exists('$WINDOW') || exists('$TMUX')
      map <leader>Y (fakeclip-screen-y)
      map <leader>P (fakeclip-screen-p)
endif

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

let g:UltiSnipsExpandTrigger="<leader><Tab>"
let g:UltiSnipsJumpForwardTrigger="<leader><Tab>"
let g:UltiSnipsJumpBackwardTrigger="<leader><s-Tab>"

"open zipped files
au BufReadCmd *.jar,*.xpi,*.wgt call zip#Browse(expand("<amatch>"))

" enable spelling
set spell
set complete+=kspell

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" go in normal mode with jj
inoremap jj <Esc>

" I18n translations
vmap <Leader>z :call I18nTranslateString()<CR>

" change in the directory of the current file
"autocmd BufEnter * :lchdir %:p:h

let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" conflict with YouCompleteMe
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

command! -bar SudoWrite :
      \ setlocal nomodified |
      \ exe (has('gui_running') ? '' : 'silent') 'write !sudo tee % >/dev/null' |
      \ let &modified = v:shell_error

cmap w!! SudoWrite

set undofile
set undoreload=10000
set undodir=$HOME/.vimundo
set backupdir =$HOME/.vim.backup
set directory=$HOME/.vim.temp

silent !mkdir -p $HOME/.vim.backup
silent !mkdir -p $HOME/.vim.temp

" Local config
if filereadable($HOME . "/.vimrc.local")
  source ~/.vimrc.local
endif
