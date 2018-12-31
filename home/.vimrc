" {{{ General
" Leader
let mapleader = " "

if has("nocompatible")
  set nocompatible " Use Vim settings, rather then Vi settings
end
set number
set mouse=a
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

" }}}

" {{{ File types
" check file change every 4 seconds ('CursorHold') and reload the buffer upon detecting change
au CursorHold * checktime

" Markdown files end in .md
au BufEnter *.md set filetype=markdown

" python setup
au BufEnter *.py set ai sw=4 ts=4 sta et fo=croql

" rust setup
au BufEnter *.rs set ai sw=4 ts=4 sta et fo=croql

" go setup
au BufEnter *.go setlocal noet ts=4 sw=4 sts=4

" tabs for C/C++ projects
"au BufEnter *.h setlocal noet ts=4 sw=4 sts=4 list!
"au BufEnter *.c setlocal noet ts=4 sw=4 sts=4 list!
"au BufEnter *.cpp setlocal noet ts=4 sw=4 sts=4 list!

"open zipped files
au BufReadCmd *.jar,*.xpi,*.wgt call zip#Browse(expand("<amatch>"))

" geojson
au BufNewFile,BufRead *.geojson set filetype=json

autocmd BufEnter * highlight Normal guibg=0
" }}}

autocmd BufNewFile,BufRead *.asd set ft=lisp

runtime macros/matchit.vim

let g:gruvbox_italic=1
set background=dark
set termguicolors
color gruvbox

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

let g:UltiSnipsSnippetsDir        = expand('~/.vim/UltiSnips')
let g:UltiSnipsSnippetDirectories = [expand('~/.vim/UltiSnips'), expand('~/.vim/vim-snippets/UltiSnips')]
" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" {{{ Go
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
" }}}

" {{{ Helper functions
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
" copy current filename:line to clipboard,
" usefull for gdb breakpoints
function! LineToClipboard()
  let path = resolve(expand(@%))
  call system('tmux load-buffer -', (path . ":" . line(".")))
endfunction
nmap <leader>l :call LineToClipboard()<CR>
" }}}

noremap <leader>a :Ack <cword><cr>
vnoremap <leader>a y:Ack <C-r>=fnameescape(@")<CR><CR>

autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

let g:syntastic_python_flake8_args='--ignore=E501,E265'

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

let g:airline_powerline_fonts = 1

" {{{ LanguageClient
let g:LanguageClient_serverCommands = {
      \ 'cpp': ['cquery'],
      \ 'c': ['cquery'],
      \ 'typescript': ['typescript-language-server'],
      \ 'python': ['pyls'],
      \ 'rust': ['rustup', 'run', 'stable', 'rls'],
      \ 'ocaml': ['ocaml-language-server', '--stdio'],
      \ 'nix': ['nix-lsp'],
      \ }

let g:LanguageClient_settingsPath = $HOME . '/.config/nvim/settings.json'
let g:LanguageClient_loadSettings = 1

set formatexpr=LanguageClient_textDocument_rangeFormatting()
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> gh :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>
nnoremap <silent> gs :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> gf :call LanguageClient_textDocument_formatting()<CR>
" }}}

" {{{ NCM2 - settings recommend by the authors
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANTE: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
" suppress the annoying 'match x of y', 'The only match' and 'Pattern not
" found' messages
set shortmess+=c

" CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
inoremap <c-c> <ESC>

" When the <Enter> key is pressed while the popup menu is visible, it only
" hides the menu. Use this mapping to close the menu and also start a new
" line.
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Press enter key to trigger snippet expansion
" The parameters are the same as `:help feedkeys()`
inoremap <silent> <expr> <CR> ncm2_ultisnips#expand_or("\<CR>", 'n')

" c-j c-k for moving in snippet
" let g:UltiSnipsExpandTrigger		= "<Plug>(ultisnips_expand)"
let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
let g:UltiSnipsRemoveSelectModeMappings = 0
" }}}

" {{{ Bracket paste (i.e. no more :set paste!)
let &t_ti .= "\<Esc>[?2004h"
let &t_te = "\e[?2004l" . &t_te

function! XTermPasteBegin(ret)
  set pastetoggle=<f29>
  set paste
  return a:ret
endfunction

execute "set <f28>=\<Esc>[200~"
execute "set <f29>=\<Esc>[201~"
map <expr> <f28> XTermPasteBegin("i")
imap <expr> <f28> XTermPasteBegin("")
vmap <expr> <f28> XTermPasteBegin("c")
cmap <f28> <nop>
cmap <f29> <nop>
" }}}

if filereadable($HOME . "/bin/osc52")
  function SendViaOSC52()
    " horrible hack but neovim seems to filter osc52 escapes
    echo system('f=mktemp; echo '.shellescape(getreg('"')).' > $f; tmux splitw -- "~/bin/osc52 < $f"; rm $f')
  endfunction
  vmap <C-c> y:call SendViaOSC52()<cr>
endif

" Local config
if filereadable($HOME . "/.vimrc.local")
  source ~/.vimrc.local
endif

if filereadable($LOCAL_VIMRC)
  source $LOCAL_VIMRC
endif

" vim: ft=vim fdm=marker
