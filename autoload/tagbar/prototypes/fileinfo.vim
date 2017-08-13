let s:FileInfo = {}

function! tagbar#prototypes#fileinfo#new(fname, ftype, typeinfo) abort
    let newobj = copy(s:FileInfo)

    " The complete file path
    let newobj.fpath = a:fname

    let newobj.bufnr = bufnr(a:fname)

    " File modification time
    let newobj.mtime = getftime(a:fname)

    " The vim file type
    let newobj.ftype = a:ftype

    " List of the tags that are present in the file, sorted according to the
    " value of 'g:tagbar_sort'
    let newobj._taglist = []
    let newobj._tagdict = {}

    " Dictionary of the tags, indexed by line number in the file
    let newobj.fline = {}

    " Dictionary of the tags, indexed by line number in the tagbar
    let newobj.tline = {}

    " Dictionary of the folding state of 'kind's, indexed by short name
    let newobj.kindfolds = {}
    let newobj.typeinfo = a:typeinfo
    " copy the default fold state from the type info
    for kind in a:typeinfo.kinds
        let newobj.kindfolds[kind.short] =
                    \ g:tagbar_foldlevel == 0 ? 1 : kind.fold
    endfor

    " Dictionary of dictionaries of the folding state of individual tags,
    " indexed by kind and full path
    let newobj.tagfolds = {}
    for kind in a:typeinfo.kinds
        let newobj.tagfolds[kind.short] = {}
    endfor

    " The current foldlevel of the file
    let newobj.foldlevel = g:tagbar_foldlevel

    return newobj
endfunction

" s:FileInfo.addTag() {{{1
function! s:FileInfo.addTag(tag) abort dict
    call add(self._taglist, a:tag)

    if has_key(self._tagdict, a:tag.name)
        call add(self._tagdict[a:tag.name], a:tag)
    else
        let self._tagdict[a:tag.name] = [a:tag]
    endif
endfunction

" s:FileInfo.getTags() {{{1
function! s:FileInfo.getTags() dict abort
    return self._taglist
endfunction

" s:FileInfo.getTagsByName() {{{1
function! s:FileInfo.getTagsByName(tagname) dict abort
    return get(self._tagdict, a:tagname, [])
endfunction

" s:FileInfo.removeTag() {{{1
function! s:FileInfo.removeTag(tag) dict abort
    let idx = index(self._taglist, a:tag)
    if idx >= 0
        call remove(self._taglist, idx)
    endif

    let namelist = get(self._tagdict, a:tag.name, [])
    let idx = index(namelist, a:tag)
    if idx >= 0
        call remove(namelist, idx)
    endif
endfunction

" s:FileInfo.reset() {{{1
" Reset stuff that gets regenerated while processing a file and save the old
" tag folds
function! s:FileInfo.reset() abort dict
    let self.mtime = getftime(self.fpath)
    let self._taglist = []
    let self._tagdict = {}
    let self.fline = {}
    let self.tline = {}

    let self._tagfolds_old = self.tagfolds
    let self.tagfolds = {}

    for kind in self.typeinfo.kinds
        let self.tagfolds[kind.short] = {}
    endfor
endfunction

" s:FileInfo.clearOldFolds() {{{1
function! s:FileInfo.clearOldFolds() abort dict
    if exists('self._tagfolds_old')
        unlet self._tagfolds_old
    endif
endfunction

" s:FileInfo.sortTags() {{{1
function! s:FileInfo.sortTags(compare_typeinfo) abort dict
    if get(a:compare_typeinfo, 'sort', g:tagbar_sort)
        call tagbar#sorting#sort(self._taglist, 'kind', a:compare_typeinfo)
    else
        call tagbar#sorting#sort(self._taglist, 'line', a:compare_typeinfo)
    endif
endfunction

" s:FileInfo.openKindFold() {{{1
function! s:FileInfo.openKindFold(kind) abort dict
    let self.kindfolds[a:kind.short] = 0
endfunction

" s:FileInfo.closeKindFold() {{{1
function! s:FileInfo.closeKindFold(kind) abort dict
    let self.kindfolds[a:kind.short] = 1
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
