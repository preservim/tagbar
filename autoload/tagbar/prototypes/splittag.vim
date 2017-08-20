" A tag that was created because of a tag name that covers multiple scopes
" Inherits the fields of the "main" tag it was split from.
" May be replaced during tag processing if it appears as a normal tag later,
" just like a pseudo tag.

let s:SplitTag = copy(g:tagbar#prototypes#normaltag#NormalTag)

function! tagbar#prototypes#splittag#new(name) abort
    let newobj = copy(s:SplitTag)

    call newobj._init(a:name)

    return newobj
endfunction

function! s:SplitTag.isSplitTag() abort dict
    return 1
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
