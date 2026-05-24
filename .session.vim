let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
doautoall SessionLoadPre
silent only
silent tabonly
cd ~/synced/projects/chunk-memo-clj
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
set shortmess+=aoO
badd +108 src/chunk_memo/coord/algebra.clj
badd +59 src/chunk_memo/coord/types.clj
badd +18 src/chunk_memo/index.clj
badd +1 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord/axis.clj
badd +17 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord.clj
badd +14 ~/synced/projects/chunk-memo-clj/src/chunk_memo/coord/ops.clj
badd +1 ~/synced/projects/chunk-memo-clj/src/chunk_memo/index/selection.clj
badd +40 test/chunk_memo/coord_test.clj
badd +35 src/chunk_memo/coord/simplify.clj
badd +113 src/chunk_memo/params.clj
badd +34 test/chunk_memo/stress_test.clj
badd +10 src/chunk_memo/chunks.clj
badd +8 src/chunk_memo/parallel.clj
badd +23 test/chunk_memo/parallel_test.clj
badd +21 src/chunk_memo/store.clj
badd +33 test/chunk_memo/cache_test.clj
badd +185 src/chunk_memo/cache.clj
badd +101 src/chunk_memo/store/filesystem.clj
badd +0 src/chunk_memo/orchestrator.clj
argglobal
%argdel
edit src/chunk_memo/orchestrator.clj
argglobal
balt src/chunk_memo/store/filesystem.clj
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
let s:l = 5 - ((4 * winheight(0) + 53) / 107)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 5
normal! 0
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
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
