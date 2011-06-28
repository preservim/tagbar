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

" Commands {{{1
command! -nargs=0 TagbarToggle        call tagbar#ToggleWindow()
command! -nargs=0 TagbarOpen          call tagbar#OpenWindow(0)
command! -nargs=0 TagbarOpenAutoClose call tagbar#OpenWindow(1)
command! -nargs=0 TagbarClose         call tagbar#CloseWindow()
command! -nargs=1 TagbarSetFoldlevel  call tagbar#SetFoldLevel(<args>)
command! -nargs=0 TagbarShowTag       call tagbar#OpenParents()

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1