" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.1
" Note:        This plugin was heavily inspired by the 'Taglist' plugin by
"              Yegappan Lakshmanan and uses a small amount of code from it.
"
" Original taglist copyright notice:
"              Permission is hereby granted to use and distribute this code,
"              with or without modifications, provided that this copyright
"              notice is copied with it. Like anything else that's free,
"              taglist.vim is provided *as is* and comes with no warranty of
"              any kind, either expressed or implied. In no event will the
"              copyright holder be liable for any damamges resulting from the
"              use of this software.
" ============================================================================

if has('multi_byte')
    scriptencoding utf-8
endif

if &cp || exists('g:loaded_tagbar')
    finish
endif

" Basic init {{{1

if v:version < 700
    echomsg 'Tagbar: Vim version is too old, Tagbar requires at least 7.0'
    finish
endif

if !exists('g:tagbar_ctags_bin')
    if executable('ctags-exuberant')
        let g:tagbar_ctags_bin = 'ctags-exuberant'
    elseif executable('exuberant-ctags')
        let g:tagbar_ctags_bin = 'exuberant-ctags'
    elseif executable('exctags')
        let g:tagbar_ctags_bin = 'exctags'
    elseif executable('ctags')
        let g:tagbar_ctags_bin = 'ctags'
    elseif executable('ctags.exe')
        let g:tagbar_ctags_bin = 'ctags.exe'
    elseif executable('tags')
        let g:tagbar_ctags_bin = 'tags'
    else
        echomsg 'Tagbar: Exuberant ctags not found, skipping plugin'
        finish
    endif
else
    let g:tagbar_ctags_bin = expand(g:tagbar_ctags_bin)
    if !executable(g:tagbar_ctags_bin)
        echomsg 'Tagbar: Exuberant ctags not found in specified place,'
              \ 'skipping plugin'
        finish
    endif
endif

redir => s:ftype_out
silent filetype
redir END
if s:ftype_out !~# 'detection:ON'
    echomsg 'Tagbar: Filetype detection is turned off, skipping plugin'
    unlet s:ftype_out
    finish
endif
unlet s:ftype_out

let g:loaded_tagbar = 1

if !exists('g:tagbar_left')
    let g:tagbar_left = 0
endif

if !exists('g:tagbar_width')
    let g:tagbar_width = 40
endif

if !exists('g:tagbar_autoclose')
    let g:tagbar_autoclose = 0
endif

if !exists('g:tagbar_autofocus')
    let g:tagbar_autofocus = 0
endif

if !exists('g:tagbar_sort')
    let g:tagbar_sort = 1
endif

if !exists('g:tagbar_compact')
    let g:tagbar_compact = 0
endif

if !exists('g:tagbar_expand')
    let g:tagbar_expand = 0
endif

if !exists('g:tagbar_singleclick')
    let g:tagbar_singleclick = 0
endif

if !exists('g:tagbar_foldlevel')
    let g:tagbar_foldlevel = 99
endif

if !exists('g:tagbar_usearrows')
    let g:tagbar_usearrows = 0
endif

if !exists('g:tagbar_autoshowtag')
    let g:tagbar_autoshowtag = 0
endif

if !exists('g:tagbar_systemenc')
    let g:tagbar_systemenc = &encoding
endif

autocmd SessionLoadPost * nested call tagbar#RestoreSession()

" Commands {{{1
command! -nargs=0 TagbarToggle        call tagbar#ToggleWindow()
command! -nargs=0 TagbarOpen          call tagbar#OpenWindow(0)
command! -nargs=0 TagbarOpenAutoClose call tagbar#OpenWindow(1)
command! -nargs=0 TagbarClose         call tagbar#CloseWindow()
command! -nargs=1 TagbarSetFoldlevel  call tagbar#SetFoldLevel(<args>)
command! -nargs=0 TagbarShowTag       call tagbar#OpenParents()

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1