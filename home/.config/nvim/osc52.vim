" Copyright (c) 2012 The Chromium OS Authors. All rights reserved.
" Use of this source code is governed by a BSD-style license that can be
" found in the LICENSE file.

"
" This script can be used to send an arbitrary string to the terminal clipboard
" using the OSC 52 escape sequence, as specified in
" http://invisible-island.net/xterm/ctlseqs/ctlseqs.html, section "Operating
" System Controls", Ps => 52.
"
" To add this script to vim...
"  1. Save it somewhere.
"  2. Edit ~/.vimrc to include...
"       source ~/path/to/osc52.vim
"       vmap <C-c> y:call SendViaOSC52(getreg('"'))<cr>
"
" This will map Ctrl-C to copy.  You can now select text in vi using the visual
" mark mode or the mouse, and press Ctrl-C to copy it to the clipboard.
"

" Max length of the OSC 52 sequence.  Sequences longer than this will not be
" sent to the terminal.
let g:max_osc52_sequence=100000

" Send a string to the terminal's clipboard using the OSC 52 sequence.
function! SendViaOSC52 (str)
  " Since tmux defaults to setting TERM=screen (ugh), we need to detect it here
  " specially.
  if !empty($TMUX)
    let osc52 = s:get_OSC52_tmux(a:str)
  elseif match($TERM, 'screen') > -1
    let osc52 = s:get_OSC52_DCS(a:str)
  else
    let osc52 = s:get_OSC52(a:str)
  endif

  let len = strlen(osc52)
  if len < g:max_osc52_sequence
    call s:rawecho(osc52)
  else
    echo "Selection too long to send to terminal: " . len
  endif
endfunction

" This function base64's the entire string and wraps it in a single OSC52.
"
" It's appropriate when running in a raw terminal that supports OSC 52.
function! s:get_OSC52 (str)
  let b64 = s:b64encode(a:str, 0)
  let rv = "\e]52;c;" . b64 . "\x07"
  return rv
endfunction

" This function base64's the entire string and wraps it in a single OSC52 for
" tmux.
"
" This is for `tmux` sessions which filters OSC 52 locally.
function! s:get_OSC52_tmux (str)
  let b64 = s:b64encode(a:str, 0)
  let rv = "\ePtmux;\e\e]52;c;" . b64 . "\x07\e\\"
  return rv
endfunction

" This function base64's the entire source, wraps it in a single OSC52, and then
" breaks the result in small chunks which are each wrapped in a DCS sequence.
"
" This is appropriate when running on `screen`.  Screen doesn't support OSC 52,
" but will pass the contents of a DCS sequence to the outer terminal unmolested.
" It imposes a small max length to DCS sequences, so we send in chunks.
function! s:get_OSC52_DCS (str)
  let b64 = s:b64encode(a:str, 76)

  " Remove the trailing newline.
  let b64 = substitute(b64, '\n*$', '', '')

  " Replace each newline with an <end-dcs><start-dcs> pair.
  let b64 = substitute(b64, '\n', "\e/\eP", "g")

  " (except end-of-dcs is "ESC \", begin is "ESC P", and I can't figure out
  "  how to express "ESC \ ESC P" in a single string.  So, the first substitute
  "  uses "ESC / ESC P", and the second one swaps out the "/".  It seems like
  "  there should be a better way.)
  let b64 = substitute(b64, '/', '\', 'g')

  " Now wrap the whole thing in <start-dcs><start-osc52>...<end-osc52><end-dcs>.
  let b64 = "\eP\e]52;c;" . b64 . "\x07\e\x5c"

  return b64
endfunction

" Echo a string to the terminal without munging the escape sequences.
"
" This function causes the terminal to flash as a side effect.  It would be
" better if it didn't, but I can't figure out how.
function! s:rawecho (str)
  exec("silent! !echo " . shellescape(a:str))
  redraw!
endfunction

" Lookup table for s:b64encode.
let s:b64_table = [
      \ "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P",
      \ "Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f",
      \ "g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v",
      \ "w","x","y","z","0","1","2","3","4","5","6","7","8","9","+","/"]

" Encode a string of bytes in base 64.
" Based on http://vim-soko.googlecode.com/svn-history/r405/trunk/vimfiles/
" autoload/base64.vim
" If size is > 0 the output will be line wrapped every `size` chars.
function! s:b64encode(str, size)
  let bytes = s:str2bytes(a:str)
  let b64 = []

  for i in range(0, len(bytes) - 1, 3)
    let n = bytes[i] * 0x10000
          \ + get(bytes, i + 1, 0) * 0x100
          \ + get(bytes, i + 2, 0)
    call add(b64, s:b64_table[n / 0x40000])
    call add(b64, s:b64_table[n / 0x1000 % 0x40])
    call add(b64, s:b64_table[n / 0x40 % 0x40])
    call add(b64, s:b64_table[n % 0x40])
  endfor

  if len(bytes) % 3 == 1
    let b64[-1] = '='
    let b64[-2] = '='
  endif

  if len(bytes) % 3 == 2
    let b64[-1] = '='
  endif

  let b64 = join(b64, '')
  if a:size <= 0
    return b64
  endif

  let chunked = ''
  while strlen(b64) > 0
    let chunked .= strpart(b64, 0, a:size) . "\n"
    let b64 = strpart(b64, a:size)
  endwhile
  return chunked
endfunction

function! s:str2bytes(str)
  return map(range(len(a:str)), 'char2nr(a:str[v:val])')
endfunction
