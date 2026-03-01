" ============================================================================
" File:        tagbar.vim
" Description: Read tags from tags file and expose current tag for statusline
" Licence:     Vim licence
" ============================================================================

scriptencoding utf-8

if &compatible || exists('g:loaded_tagbar')
    finish
endif

" Basic init {{{1

if v:version < 700
    echohl WarningMsg
    echomsg 'Tagbar: Vim version is too old, Tagbar requires at least 7.0'
    echohl None
    finish
endif

function! s:init_var(var, value) abort
    if !exists('g:tagbar_' . a:var)
        execute 'let g:tagbar_' . a:var . ' = ' . string(a:value)
    endif
endfunction

function! s:setup_options() abort
    let options = [
        \ ['foldlevel', 99],
        \ ['highlight_method', 'nearest-stl'],
        \ ['sort', 1],
    \ ]

    for [opt, val] in options
        call s:init_var(opt, val)
        unlet val
    endfor
endfunction
call s:setup_options()

" Commands {{{1
command! -nargs=* TagbarCurrentTag echo tagbar#currenttag('%s', 'No current tag', <f-args>)

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
