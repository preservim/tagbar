let s:TypeInfo = {}

function! tagbar#prototypes#typeinfo#new(...) abort
    let newobj = copy(s:TypeInfo)

    let newobj.kinddict = {}

    if a:0 > 0
        call extend(newobj, a:1)
    endif

    return newobj
endfunction

" s:TypeInfo.getKind() {{{1
function! s:TypeInfo.getKind(kind) abort dict
    let idx = self.kinddict[a:kind]
    return self.kinds[idx]
endfunction

" s:TypeInfo.createKinddict() {{{1
" Create a dictionary of the kind order for fast access in sorting functions
function! s:TypeInfo.createKinddict() abort dict
    let i = 0
    for kind in self.kinds
        let self.kinddict[kind.short] = i
        let i += 1
    endfor
    let self.kinddict['?'] = i
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
