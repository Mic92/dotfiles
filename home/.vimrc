let s:python_interpreter = 'python2'

" Leader
let mapleader = " "

if has("nocompatible")
  set nocompatible " Use Vim settings, rather then Vi settings
end
set mouse=
set ruler " show the cursor position all the time
set cursorline " highlight current line
set showcmd " display incomplete commands
set incsearch " do incremental searching
set laststatus=2 " Always display the status line
set smartcase
set ignorecase
set showmatch     " Show matching bracets when text indicator is over them
set autowrite     " write a modified buffer on each :next ,  ...

set modeline      " React on modlines in files
set hlsearch      " highlightthe last used search pattern
set nojoinspaces
set autoread

set wildignore=*.bak,*.o,*.e,*~      " wildmenu: ignore these extensions
set iskeyword+=_,-  " these characters also belong to a word
set wrap                             " do wrap lines
set noerrorbells                     " disable error bells
set novisualbell                     " disable beep
set backspace=indent,eol,start       " backspacing over everything in insert mode
set browsedir=current                " which directory to use for the file browser
set shell=bash
" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab
set textwidth=80

if ! $SSH_CONNECTION
  set clipboard+=unnamedplus
endif

set undofile
set undoreload=10000
set undodir=$HOME/.vimundo
set backupdir =$HOME/.vim.backup
set directory=$HOME/.vim.temp
silent !mkdir -p "$HOME/.vim.temp" "$HOME/.vim.backup"

" enable spelling
set spell
set complete+=kspell

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

if &termencoding == ""
  let &termencoding = &encoding
endif
set encoding=utf-8
setglobal fileencodings=ucs-bom,utf-8,default,latin1

filetype plugin indent on
syntax on

runtime macros/matchit.vim

colorscheme solarized

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


map <C-n> :NERDTreeToggle<CR>

" Use Ag (https://github.com/ggreer/the_silver_searcher) instead of Grep when
" available
if executable('ag')
" Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

" ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

endif
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_dont_split = 'NERD_tree_2'
" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" check file change every 4 seconds ('CursorHold') and reload the buffer upon detecting change
au CursorHold * checktime

" Markdown files end in .md
au BufEnter *.md set filetype=markdown

" python setup
au BufEnter *.py set ai sw=4 ts=4 sta et fo=croql

" rust setup
au BufEnter *.rs set ai sw=4 ts=4 sta et fo=croql

" go setup
au BufEnter *.go setlocal noet ts=4 sw=4 sts=4 list!


" tabs for C/C++ projects
"au BufEnter *.h setlocal noet ts=4 sw=4 sts=4 list!
"au BufEnter *.c setlocal noet ts=4 sw=4 sts=4 list!
"au BufEnter *.cpp setlocal noet ts=4 sw=4 sts=4 list!

let g:UltiSnipsSnippetsDir        = expand('~/.vim/UltiSnips')
let g:UltiSnipsSnippetDirectories = [expand('~/.vim/UltiSnips'), expand('~/.vim/vim-snippets/UltiSnips')]

"open zipped files
au BufReadCmd *.jar,*.xpi,*.wgt call zip#Browse(expand("<amatch>"))

" geojson
au BufNewFile,BufRead *.geojson set filetype=json

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

" Repeat "." in visual mode
vnoremap . :norm.<CR>

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

command! -bar SudoWrite :
      \ setlocal nomodified |
      \ exe (has('gui_running') ? '' : 'silent') 'write !sudo tee % >/dev/null' |
      \ let &modified = v:shell_error

cmap w!! SudoWrite

if has("python")
  :command! -nargs=+ Calc :py print <args>
  :py from math import *
else
  command! -nargs=+ Calc :!python -c "from math import *; print <args>"
end

au FileType go nmap <Leader>s <Plug>(go-implements)
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <Leader>gb <Plug>(go-doc-browser)

au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)

au FileType go nmap <Leader>e <Plug>(go-rename)

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

"let g:deoplete#enable_at_startup = 1
"call deoplete#enable()
"" deoplete-go settings
"let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
"let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
"let g:deoplete#sources#go#use_cache = 1
"let g:deoplete#sources = {}
"let g:deoplete#sources._ = ['buffer', 'file', 'omni', 'ultisnips']
"let g:deoplete#sources.python = ['jedi']
"let g:deoplete#sources#python#use_cache = 1

let g:python_host_prog="/run/current-system/sw/bin/nvim-python"
let g:python3_host_prog="/run/current-system/sw/bin/nvim-python3"

let g:rustfmt_autosave = 1

" copy current filename:line to clipboard,
" usefull for gdb breakpoints
function! LineToClipboard()
  let path = resolve(expand(@%))
  call system('xclip', (path . ":" . line(".")))
endfunction
nmap <leader>l :call LineToClipboard()<CR>

" Local config
if filereadable($HOME . "/.vimrc.local")
  source ~/.vimrc.local
endif

if filereadable($LOCAL_VIMRC)
  source $LOCAL_VIMRC
endif

nnoremap <leader>jd :YcmCompleter GoToDeclaration<CR>
nnoremap <leader>je :YcmCompleter GoToDefinition<CR>
nnoremap <leader>jf :YcmCompleter FixIt<CR>
nnoremap <leader>jt :YcmCompleter GetType<CR>

let g:syntastic_python_flake8_args='--ignore=E501,E265'

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
noremap <Leader>a :Ack <cword><cr>
