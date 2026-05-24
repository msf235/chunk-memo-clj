let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/synced/projects/chunk-memo-clj
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +108 src/chunk_memo/coord/algebra.clj
badd +59 src/chunk_memo/coord/types.clj
badd +2 src/chunk_memo/index.clj
badd +1 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord/axis.clj
badd +15 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord.clj
badd +14 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord/ops.clj
badd +5 ~/synced/projects/chunk-memo-clj/src/chunk_memo/index/selection.clj
badd +8 test/chunk_memo/coord_test.clj
badd +14 src/chunk_memo/layout.clj
badd +1 src/chunk_memo/scratch.clj
badd +38 src/chunk_memo/bitmap.clj
badd +13 deps.edn
badd +1 diffview:///Users/matthew/synced/projects/chunk-memo-clj/.git/:0:/src/chunk_memo/bitmap.clj
badd +1 diffview:///panels/0/DiffviewFilePanel
badd +163 src/chunk_memo/cache.clj
argglobal
%argdel
edit src/chunk_memo/cache.clj
argglobal
balt src/chunk_memo/bitmap.clj
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 75 - ((20 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 75
normal! 039|
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
