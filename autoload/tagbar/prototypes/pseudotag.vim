let s:PseudoTag = copy(g:tagbar#prototypes#basetag#BaseTag)

function! tagbar#prototypes#pseudotag#new(name) abort
    let newobj = copy(s:PseudoTag)

    call newobj._init(a:name)

    return newobj
endfunction

" s:PseudoTag.isPseudoTag() {{{1
function! s:PseudoTag.isPseudoTag() abort dict
    return 1
endfunction

" s:PseudoTag.strfmt() {{{1
function! s:PseudoTag.strfmt() abort dict
    let typeinfo = self.typeinfo

    let suffix = get(self.fields, 'signature', '')
    if has_key(typeinfo.kind2scope, self.fields.kind)
        let suffix .= ' : ' . typeinfo.kind2scope[self.fields.kind]
    endif

    return self._getPrefix() . self.name . '*' . suffix
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
