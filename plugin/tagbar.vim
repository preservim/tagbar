" ============================================================================
" File:        tagbar.vim
" Description: List the current file's tags in a sidebar, ordered by class etc
" Author:      Jan Larres <jan@majutsushi.net>
" Licence:     Vim licence
" Website:     http://majutsushi.github.com/tagbar/
" Version:     2.4.1
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

scriptencoding utf-8

if &cp || exists('g:loaded_tagbar')
    finish
endif

" Basic init {{{1

if v:version < 700
    echohl WarningMsg
    echomsg 'Tagbar: Vim version is too old, Tagbar requires at least 7.0'
    echohl None
    finish
endif

if v:version == 700 && !has('patch167')
    echohl WarningMsg
    echomsg 'Tagbar: Vim versions lower than 7.0.167 have a bug'
          \ 'that prevents this version of Tagbar from working.'
          \ 'Please use the alternate version posted on the website.'
    echohl None
    finish
endif

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

if !exists('g:tagbar_iconchars')
    if has('multi_byte') && has('unix') && &encoding == 'utf-8' &&
     \ (empty(&termencoding) || &termencoding == 'utf-8')
        let g:tagbar_iconchars = ['▶', '▼']
    else
        let g:tagbar_iconchars = ['+', '-']
    endif
endif

if !exists('g:tagbar_autoshowtag')
    let g:tagbar_autoshowtag = 0
endif

if !exists('g:tagbar_updateonsave_maxlines')
    let g:tagbar_updateonsave_maxlines = 5000
endif

if !exists('g:tagbar_systemenc')
    let g:tagbar_systemenc = &encoding
endif

augroup TagbarSession
    autocmd!
    autocmd SessionLoadPost * nested call tagbar#RestoreSession()
augroup END

" Commands {{{1
command! -nargs=0 TagbarToggle        call tagbar#ToggleWindow()
command! -nargs=? TagbarOpen          call tagbar#OpenWindow(<f-args>)
command! -nargs=0 TagbarOpenAutoClose call tagbar#OpenWindow('fc')
command! -nargs=0 TagbarClose         call tagbar#CloseWindow()
command! -nargs=1 -bang TagbarSetFoldlevel  call tagbar#SetFoldLevel(<args>, <bang>0)
command! -nargs=0 TagbarShowTag       call tagbar#OpenParents()
command! -nargs=1 TagbarGetTypeConfig call tagbar#gettypeconfig(<f-args>)
command! -nargs=? TagbarDebug         call tagbar#StartDebug(<f-args>)
command! -nargs=0 TagbarDebugEnd      call tagbar#StopDebug()

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
