let s:visibility_symbols = {
    \ 'public'    : '+',
    \ 'protected' : '#',
    \ 'private'   : '-'
\ }

let s:BaseTag = {}

let g:tagbar#prototypes#basetag#BaseTag = s:BaseTag

function! tagbar#prototypes#basetag#new(name) abort
    let newobj = copy(s:BaseTag)

    call newobj._init(a:name)

    return newobj
endfunction

" s:BaseTag._init() {{{1
function! s:BaseTag._init(name) abort dict
    let self.name          = a:name
    let self.fields        = {}
    let self.fields.line   = 0
    let self.fields.column = 0
    let self.prototype     = ''
    let self.path          = ''
    let self.fullpath      = a:name
    let self.depth         = 0
    let self.parent        = {}
    let self.tline         = -1
    let self.fileinfo      = {}
    let self.typeinfo      = {}
    let self._childlist    = []
    let self._childdict    = {}
endfunction

" s:BaseTag.isNormalTag() {{{1
function! s:BaseTag.isNormalTag() abort dict
    return 0
endfunction

" s:BaseTag.isPseudoTag() {{{1
function! s:BaseTag.isPseudoTag() abort dict
    return 0
endfunction

" s:BaseTag.isSplitTag {{{1
function! s:BaseTag.isSplitTag() abort dict
    return 0
endfunction

" s:BaseTag.isKindheader() {{{1
function! s:BaseTag.isKindheader() abort dict
    return 0
endfunction

" s:BaseTag.getPrototype() {{{1
function! s:BaseTag.getPrototype(short) abort dict
    return self.prototype
endfunction

" s:BaseTag._getPrefix() {{{1
function! s:BaseTag._getPrefix() abort dict
    let fileinfo = self.fileinfo

    if !empty(self._childlist)
        if fileinfo.tagfolds[self.fields.kind][self.fullpath]
            let prefix = g:tagbar#icon_closed
        else
            let prefix = g:tagbar#icon_open
        endif
    else
        let prefix = ' '
    endif
    " Visibility is called 'access' in the ctags output
    if g:tagbar_show_visibility
        if has_key(self.fields, 'access')
            let prefix .= get(s:visibility_symbols, self.fields.access, ' ')
        elseif has_key(self.fields, 'file')
            let prefix .= s:visibility_symbols.private
        else
            let prefix .= ' '
        endif
    endif

    return prefix
endfunction

" s:BaseTag.initFoldState() {{{1
function! s:BaseTag.initFoldState(known_files) abort dict
    let fileinfo = self.fileinfo

    if a:known_files.has(fileinfo.fpath) &&
     \ has_key(fileinfo, '_tagfolds_old') &&
     \ has_key(fileinfo._tagfolds_old[self.fields.kind], self.fullpath)
        " The file has been updated and the tag was there before, so copy its
        " old fold state
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo._tagfolds_old[self.fields.kind][self.fullpath]
    elseif self.depth >= fileinfo.foldlevel
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
    else
        let fileinfo.tagfolds[self.fields.kind][self.fullpath] =
                    \ fileinfo.kindfolds[self.fields.kind]
    endif
endfunction

" s:BaseTag.getClosedParentTline() {{{1
function! s:BaseTag.getClosedParentTline() abort dict
    let tagline  = self.tline

    " Find the first closed parent, starting from the top of the hierarchy.
    let parents   = []
    let curparent = self.parent
    while !empty(curparent)
        call add(parents, curparent)
        let curparent = curparent.parent
    endwhile
    for parent in reverse(parents)
        if parent.isFolded()
            let tagline = parent.tline
            break
        endif
    endfor

    return tagline
endfunction

" s:BaseTag.isFoldable() {{{1
function! s:BaseTag.isFoldable() abort dict
    return !empty(self._childlist)
endfunction

" s:BaseTag.isFolded() {{{1
function! s:BaseTag.isFolded() abort dict
    return self.fileinfo.tagfolds[self.fields.kind][self.fullpath]
endfunction

" s:BaseTag.openFold() {{{1
function! s:BaseTag.openFold() abort dict
    if self.isFoldable()
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 0
    endif
endfunction

" s:BaseTag.closeFold() {{{1
function! s:BaseTag.closeFold() abort dict
    let newline = line('.')

    if !empty(self.parent) && self.parent.isKindheader()
        " Tag is child of generic 'kind'
        call self.parent.closeFold()
        let newline = self.parent.tline
    elseif self.isFoldable() && !self.isFolded()
        " Tag is parent of a scope and is not folded
        let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = 1
        let newline = self.tline
    elseif !empty(self.parent)
        " Tag is normal child, so close parent
        let parent = self.parent
        let self.fileinfo.tagfolds[parent.fields.kind][parent.fullpath] = 1
        let newline = parent.tline
    endif

    return newline
endfunction

" s:BaseTag.setFolded() {{{1
function! s:BaseTag.setFolded(folded) abort dict
    let self.fileinfo.tagfolds[self.fields.kind][self.fullpath] = a:folded
endfunction

" s:BaseTag.openParents() {{{1
function! s:BaseTag.openParents() abort dict
    let parent = self.parent

    while !empty(parent)
        call parent.openFold()
        let parent = parent.parent
    endwhile
endfunction

" s:BaseTag.addChild() {{{1
function! s:BaseTag.addChild(tag) abort dict
    call add(self._childlist, a:tag)

    if has_key(self._childdict, a:tag.name)
        call add(self._childdict[a:tag.name], a:tag)
    else
        let self._childdict[a:tag.name] = [a:tag]
    endif
endfunction

" s:BaseTag.getChildren() {{{1
function! s:BaseTag.getChildren() dict abort
    return self._childlist
endfunction

" s:BaseTag.getChildrenByName() {{{1
function! s:BaseTag.getChildrenByName(tagname) dict abort
    return get(self._childdict, a:tagname, [])
endfunction

" s:BaseTag.removeChild() {{{1
function! s:BaseTag.removeChild(tag) dict abort
    let idx = index(self._childlist, a:tag)
    if idx >= 0
        call remove(self._childlist, idx)
    endif

    let namelist = get(self._childdict, a:tag.name, [])
    let idx = index(namelist, a:tag)
    if idx >= 0
        call remove(namelist, idx)
    endif
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
