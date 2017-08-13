let s:KindheaderTag = copy(g:tagbar#prototypes#basetag#BaseTag)

function! tagbar#prototypes#kindheadertag#new(name) abort
    let newobj = copy(s:KindheaderTag)

    call newobj._init(a:name)

    return newobj
endfunction

" s:KindheaderTag.isKindheader() {{{1
function! s:KindheaderTag.isKindheader() abort dict
    return 1
endfunction

" s:KindheaderTag.getPrototype() {{{1
function! s:KindheaderTag.getPrototype(short) abort dict
    return self.name . ': ' .
         \ self.numtags . ' ' . (self.numtags > 1 ? 'tags' : 'tag')
endfunction

" s:KindheaderTag.isFoldable() {{{1
function! s:KindheaderTag.isFoldable() abort dict
    return 1
endfunction

" s:KindheaderTag.isFolded() {{{1
function! s:KindheaderTag.isFolded() abort dict
    return self.fileinfo.kindfolds[self.short]
endfunction

" s:KindheaderTag.openFold() {{{1
function! s:KindheaderTag.openFold() abort dict
    let self.fileinfo.kindfolds[self.short] = 0
endfunction

" s:KindheaderTag.closeFold() {{{1
function! s:KindheaderTag.closeFold() abort dict
    let self.fileinfo.kindfolds[self.short] = 1
    return line('.')
endfunction

" s:KindheaderTag.toggleFold() {{{1
function! s:KindheaderTag.toggleFold(fileinfo) abort dict
    let a:fileinfo.kindfolds[self.short] = !a:fileinfo.kindfolds[self.short]
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
