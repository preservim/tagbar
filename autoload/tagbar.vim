" ============================================================================
" File:        tagbar.vim
" Description: Read tags from tags file and expose current tag for statusline
" Licence:     Vim licence
" ============================================================================

scriptencoding utf-8

" Initialization {{{1

if exists(':TagbarCurrentTag') == 0
    runtime plugin/tagbar.vim
endif

" Basic init {{{2

redir => s:ftype_out
silent filetype
redir END
if s:ftype_out !~# 'detection:ON'
    echomsg 'Tagbar: Filetype detection is turned off, skipping plugin'
    unlet s:ftype_out
    finish
endif
unlet s:ftype_out

let s:type_init_done    = 0
let s:autocommands_done = 0
let s:init_done = 0
let s:nearby_disabled = 0
let s:statusline_in_use = 0

let g:loaded_tagbar = 1

let s:warnings = {
    \ 'type': [],
\ }

let s:singular_types = {
            \ 'Classes': 'Class',
            \ 'Delegates': 'Delegate',
            \ 'Enumeration values': 'Enumeration value',
            \ 'Enumerations': 'Enumeration',
            \ 'Error codes': 'Error code',
            \ 'Error domains': 'Error domain',
            \ 'Fields': 'Field',
            \ 'Interfaces': 'Interface',
            \ 'JavaScript funtions': 'JavaScript function',
            \ 'Methods': 'Method',
            \ 'MobiLink Conn Scripts': 'MobiLink Conn Script',
            \ 'MobiLink Properties': 'MobiLink Property',
            \ 'MobiLink Table Scripts': 'MobiLink Table Script',
            \ 'Properties': 'Property',
            \ 'Signals': 'Signal',
            \ 'Structures': 'Structure',
            \ 'autocommand groups': 'autocommand group',
            \ 'block data': 'block data',
            \ 'block label': 'block label',
            \ 'chapters': 'chapter',
            \ 'classes': 'class',
            \ 'commands': 'command',
            \ 'common blocks': 'common block',
            \ 'components': 'component',
            \ 'constant definitions': 'constant definition',
            \ 'constants': 'constant',
            \ 'constructors': 'constructor',
            \ 'cursors': 'cursor',
            \ 'data items': 'data item',
            \ 'defines': 'define',
            \ 'derived types and structures': 'derived type and structure',
            \ 'domains': 'domain',
            \ 'entities': 'entity',
            \ 'entry points': 'entry point',
            \ 'embedded': 'embedded',
            \ 'enum constants': 'enum constant',
            \ 'enum types': 'enum type',
            \ 'enumerations': 'enumeration',
            \ 'enumerators': 'enumerator',
            \ 'enums': 'enum',
            \ 'events': 'event',
            \ 'exception declarations': 'exception declaration',
            \ 'exceptions': 'exception',
            \ 'features': 'feature',
            \ 'fields': 'field',
            \ 'file descriptions': 'file description',
            \ 'formats': 'format',
            \ 'fragments': 'fragment',
            \ 'function definitions': 'function definition',
            \ 'functions': 'function',
            \ 'functor definitions': 'functor definition',
            \ 'global variables': 'global variable',
            \ 'group items': 'group item',
            \ 'imports': 'import',
            \ 'includes': 'include',
            \ 'indexes': 'index',
            \ 'interfaces': 'interface',
            \ 'javascript functions': 'JavaScript function',
            \ 'labels': 'label',
            \ 'macro definitions': 'macro definition',
            \ 'macros': 'macro',
            \ 'maps': 'map',
            \ 'members': 'member',
            \ 'methods': 'method',
            \ 'modules or functors': 'module or function',
            \ 'modules': 'module',
            \ 'mxtags': 'mxtag',
            \ 'named anchors': 'named anchor',
            \ 'namelists': 'namelist',
            \ 'namespaces': 'namespace',
            \ 'net data types': 'net data type',
            \ 'packages': 'package',
            \ 'package': 'package',
            \ 'paragraphs': 'paragraph',
            \ 'parts': 'part',
            \ 'patterns': 'pattern',
            \ 'ports': 'port',
            \ 'procedures': 'procedure',
            \ 'program ids': 'program id',
            \ 'programs': 'program',
            \ 'projects': 'project',
            \ 'properties': 'property',
            \ 'prototypes': 'prototype',
            \ 'publications': 'publication',
            \ 'record definitions': 'record definition',
            \ 'record fields': 'record field',
            \ 'records': 'record',
            \ 'register data types': 'register data type',
            \ 'sections': 'section',
            \ 'services': 'services',
            \ 'sets': 'sets',
            \ 'signature declarations': 'signature declaration',
            \ 'singleton methods': 'singleton method',
            \ 'slots': 'slot',
            \ 'structs': 'struct',
            \ 'structure declarations': 'structure declaration',
            \ 'structure fields': 'structure field',
            \ 'subparagraphs': 'subparagraph',
            \ 'subroutines': 'subroutine',
            \ 'subsections': 'subsection',
            \ 'subsubsections': 'subsubsection',
            \ 'subtypes': 'subtype',
            \ 'synonyms': 'synonym',
            \ 'tables': 'table',
            \ 'targets': 'target',
            \ 'tasks': 'task',
            \ 'triggers': 'trigger',
            \ 'type definitions': 'type definition',
            \ 'type names': 'type name',
            \ 'typedefs': 'typedef',
            \ 'types': 'type',
            \ 'unions': 'union',
            \ 'value bindings': 'value binding',
            \ 'variables': 'variable',
            \ 'views': 'view',
            \ 'vimball filenames': 'vimball filename'}

" s:Init() {{{2
function! s:Init(silent) abort
    if !s:type_init_done
        call s:InitTypes()
    endif

    if !s:autocommands_done
        call s:CreateAutocommands()
        call s:AutoUpdate(fnamemodify(expand('%'), ':p'), 0)
    endif

    let s:init_done = 1
    return 1
endfunction

" s:InitTypes() {{{2
function! s:InitTypes() abort
    call tagbar#debug#log('Initializing types')

    " Load both type sets; we don't know which ctags generated the tags file
    let s:known_types = tagbar#types#uctags#init({})

    " Merge in ctags types for anything uctags didn't define
    let ctags_types = tagbar#types#ctags#init({})
    for [ftype, typeinfo] in items(ctags_types)
        if !has_key(s:known_types, ftype)
            let s:known_types[ftype] = typeinfo
        endif
    endfor

    " Create the kind dictionaries for all types
    for typeinfo in values(s:known_types)
        call typeinfo.createKinddict()
    endfor

    " Apply user-defined type overrides
    call s:LoadUserTypeDefs()

    let s:type_init_done = 1
endfunction

" s:LoadUserTypeDefs() {{{2
function! s:LoadUserTypeDefs(...) abort
    if a:0 > 0
        let ftypes = [a:1]
    else
        let ftypes = keys(s:known_types)
    endif

    for ftype in ftypes
        if exists('g:tagbar_type_' . ftype)
            let userdef = g:tagbar_type_{ftype}
            if has_key(userdef, 'kinds')
                let userdef = s:TransformUserTypeDef(userdef)
                let s:known_types[ftype] = tagbar#prototypes#typeinfo#new(userdef)
                call s:known_types[ftype].createKinddict()
            endif
        endif
    endfor
endfunction

" s:TransformUserTypeDef() {{{2
function! s:TransformUserTypeDef(def) abort
    let newdef = copy(a:def)
    if has_key(newdef, 'kinds')
        let newkinds = []
        for kind in newdef.kinds
            let kindparts = split(kind, ':')
            let newkind = {'short': kindparts[0], 'long': kindparts[1],
                         \ 'fold': 0, 'stl': 1}
            if len(kindparts) > 2
                let newkind.fold = kindparts[2]
            endif
            if len(kindparts) > 3
                let newkind.stl = kindparts[3]
            endif
            call add(newkinds, newkind)
        endfor
        let newdef.kinds = newkinds
    endif
    return newdef
endfunction

" s:CreateAutocommands() {{{2
function! s:CreateAutocommands() abort
    call tagbar#debug#log('Creating autocommands')

    augroup TagbarAutoCmds
        autocmd!
        autocmd BufReadPost,BufEnter,CursorHold,FileType * call
                    \ s:AutoUpdate(fnamemodify(expand('<afile>'), ':p'), 0)
        autocmd BufDelete,BufWipeout *
                    \ nested call s:known_files.rm(fnamemodify(expand('<afile>'), ':p'))
    augroup END

    let s:autocommands_done = 1
endfunction

" Known files cache {{{1

let s:known_files = {
    \ '_files'   : {}
\ }

function! s:known_files.get(fname) abort dict
    return get(self._files, a:fname, {})
endfunction

function! s:known_files.put(fileinfo, ...) abort dict
    if a:0 == 1
        let self._files[a:1] = a:fileinfo
    else
        let fname = a:fileinfo.fpath
        let self._files[fname] = a:fileinfo
    endif
endfunction

function! s:known_files.has(fname) abort dict
    return has_key(self._files, a:fname)
endfunction

function! s:known_files.rm(fname) abort dict
    if s:known_files.has(a:fname)
        call tagbar#debug#log('Removing fileinfo for [' . a:fname . ']')
        call remove(self._files, a:fname)
    endif
endfunction

" Tags file reading {{{1

" s:FindTagsFile() {{{2
" Use Vim's built-in tagfiles() which respects the 'tags' option
function! s:FindTagsFile() abort
    let tfiles = tagfiles()
    for f in tfiles
        if filereadable(f)
            call tagbar#debug#log('Found tags file: ' . f)
            return fnamemodify(f, ':p')
        endif
    endfor
    call tagbar#debug#log('No tags file found')
    return ''
endfunction

" s:ReadTagsForFile() {{{2
" Read tags from a tags file, filtering for the given source file
function! s:ReadTagsForFile(tagsfile, fname) abort
    call tagbar#debug#log('Reading tags for [' . a:fname . '] from [' . a:tagsfile . ']')

    let tagsdir = fnamemodify(a:tagsfile, ':p:h')

    " Compute the relative path of the source file from the tags file dir
    let abspath = fnamemodify(a:fname, ':p')
    let relpath = ''
    if tagsdir[-1:] !=# '/'
        let tagsdir_slash = tagsdir . '/'
    else
        let tagsdir_slash = tagsdir
    endif
    if strpart(abspath, 0, len(tagsdir_slash)) ==# tagsdir_slash
        let relpath = strpart(abspath, len(tagsdir_slash))
    endif

    let lines = readfile(a:tagsfile)
    let result = []
    for line in lines
        " Skip metadata lines
        if line[0] ==# '!'
            continue
        endif

        " Quick check: the filename field (second tab-separated column) must
        " match either the relative or absolute path
        let first_tab = stridx(line, "\t")
        if first_tab == -1
            continue
        endif
        let second_tab = stridx(line, "\t", first_tab + 1)
        if second_tab == -1
            continue
        endif
        let tag_fname = strpart(line, first_tab + 1, second_tab - first_tab - 1)

        if tag_fname ==# relpath || tag_fname ==# abspath
            call add(result, line)
        endif
    endfor

    call tagbar#debug#log('Found ' . len(result) . ' tags for file')
    return join(result, "\n")
endfunction

" File processing {{{1

" s:ProcessFile() {{{2
function! s:ProcessFile(fname, ftype) abort
    call tagbar#debug#log('ProcessFile called [' . a:fname . ']')

    if !s:IsValidFile(a:fname, a:ftype)
        call tagbar#debug#log('Not a valid file, returning')
        return
    endif

    let typeinfo = s:known_types[a:ftype]

    " If the file has only been updated preserve the fold states, otherwise
    " create a new entry
    if s:known_files.has(a:fname) && !empty(s:known_files.get(a:fname)) &&
     \ s:known_files.get(a:fname).ftype == a:ftype
        let fileinfo = s:known_files.get(a:fname)
        let typeinfo = fileinfo.typeinfo
        call fileinfo.reset()
    else
        let fileinfo = tagbar#prototypes#fileinfo#new(a:fname, a:ftype, typeinfo)
    endif

    " Find and read from the tags file
    let tagsfile = s:FindTagsFile()
    if tagsfile ==# ''
        call tagbar#debug#log('No tags file found')
        call s:known_files.put(tagbar#prototypes#fileinfo#new(a:fname, a:ftype,
                                            \ s:known_types[a:ftype]), a:fname)
        return
    endif

    let tags_output = s:ReadTagsForFile(tagsfile, a:fname)

    if tags_output ==# ''
        call tagbar#debug#log('No tags found for file')
        call s:known_files.put(tagbar#prototypes#fileinfo#new(a:fname, a:ftype,
                                            \ s:known_types[a:ftype]), a:fname)
        return
    endif

    call tagbar#debug#log('Filetype tag kinds: ' . string(keys(typeinfo.kinddict)))

    " Parse the tag lines
    call tagbar#debug#log('Parsing tags')
    let rawtaglist = split(tags_output, '\n\+')
    let seen = {}
    for line in rawtaglist
        if has_key(seen, line)
            continue
        endif
        let seen[line] = 1

        let parts = split(line, ';"\t')
        if len(parts) >= 2
            " If the excmd pattern itself contained ;"<TAB>, rejoin the
            " leading parts. The last part is always the fields section.
            let fieldpart = parts[-1]
            let tagpart = join(parts[0:-2], ';"' . "\t")
            call s:ParseTagline(tagpart, fieldpart, typeinfo, fileinfo)
        endif
    endfor

    " Create kind header tags for non-scoped tags
    for kind in typeinfo.kinds
        if has_key(get(typeinfo, 'kind2scope', {}), kind.short)
            continue
        endif

        let curtags = filter(copy(fileinfo.getTags()),
                           \ 'v:val.fields.kind ==# kind.short && ' .
                           \ '!has_key(v:val, "scope")')

        if empty(curtags)
            continue
        endif

        let kindtag          = tagbar#prototypes#kindheadertag#new(kind.long)
        let kindtag.short    = kind.short
        let kindtag.numtags  = len(curtags)
        let kindtag.fileinfo = fileinfo

        for tag in curtags
            let tag.parent = kindtag
        endfor
    endfor

    " Clear old folding information
    call fileinfo.clearOldFolds()

    " Sort the tags
    call fileinfo.sortTags(typeinfo)

    call s:known_files.put(fileinfo)
endfunction

" s:ParseTagline() {{{2
" Structure of a tag line:
" tagname<TAB>filename<TAB>expattern;"fields
" fields: <TAB>name:value
function! s:ParseTagline(part1, part2, typeinfo, fileinfo) abort
    let basic_info  = split(a:part1, '\t')
    if len(basic_info) < 3
        return
    endif

    let tagname  = basic_info[0]

    " the pattern can contain tabs and thus may have been split up, so join
    " the rest of the items together again
    let pattern = join(basic_info[2:], "\t")
    let excmd_line = 0
    if pattern[0] ==# '/'
        let start   = 2 " skip the slash and the ^
        let end     = strlen(pattern) - 1
        if pattern[end - 1] ==# '$'
            let end -= 1
            let dollar = '\$'
        else
            let dollar = ''
        endif
        let pattern = '\V\^\C' . strpart(pattern, start, end - start) . dollar
    else
        " The excmd is a line number, not a pattern
        if pattern =~# '^\d\+$'
            let excmd_line = str2nr(pattern)
        endif
        let pattern = ''
    endif

    " When splitting fields make sure not to create empty keys or values
    let fields = split(a:part2, '\t\ze\w\+:')
    let fielddict = {}
    if fields[0] !~# ':'
        let fielddict.kind = remove(fields, 0)
    endif
    for field in fields
        let delimit = stridx(field, ':')
        let key = strpart(field, 0, delimit)
        let val = substitute(strpart(field, delimit + 1), '\t', '', 'g')
        if key ==# 'file'
            let fielddict[key] = 'yes'
        endif
        if len(val) > 0
            if key ==# 'line' || key ==# 'column' || key ==# 'end'
                let fielddict[key] = str2nr(val)
            else
                let fielddict[key] = val
            endif
        endif
    endfor

    " If the excmd was a line number and no explicit line: field was found,
    " use the excmd line number as a fallback
    if excmd_line > 0 && !has_key(fielddict, 'line')
        let fielddict.line = excmd_line
    endif

    " If the tag covers multiple scopes, split it up
    if has_key(a:typeinfo, 'kind2scope') && has_key(fielddict, 'kind')
                \ && has_key(a:typeinfo.kind2scope, fielddict.kind)
                \ && tagname =~# '\V' . escape(a:typeinfo.sro, '\')
        let tagparts = split(tagname, '\V' . escape(a:typeinfo.sro, '\'))

        let scope = a:typeinfo.kind2scope[fielddict.kind]
        if has_key(fielddict, scope)
            let parent = fielddict[scope]
        else
            let parent = ''
        endif
        let curfielddict = fielddict

        for i in range(len(tagparts))
            let part = tagparts[i]
            call s:ProcessTag(part, pattern, curfielddict,
                            \ i != len(tagparts) - 1, a:typeinfo, a:fileinfo)
            if parent !=# ''
                let parent = parent . a:typeinfo.sro . part
            else
                let parent = part
            endif
            let curfielddict = copy(fielddict)
            let curfielddict[scope] = parent
        endfor
    else
        call s:ProcessTag(tagname, pattern, fielddict, 0,
                        \ a:typeinfo, a:fileinfo)
    endif
endfunction

" s:ProcessTag() {{{2
function! s:ProcessTag(name, pattern, fields, is_split, typeinfo, fileinfo) abort
    if a:is_split
        let taginfo = tagbar#prototypes#splittag#new(a:name)
    else
        let taginfo = tagbar#prototypes#normaltag#new(a:name)
    endif

    let taginfo.pattern = a:pattern
    call extend(taginfo.fields, a:fields)

    " Needed for jsctags
    if has_key(taginfo.fields, 'lineno')
        let taginfo.fields.line = str2nr(taginfo.fields.lineno)
    endif
    if taginfo.fields.line < 0
        let taginfo.fields.line = 0
    endif

    " Make sure our 'end' is valid
    if taginfo.fields.end < taginfo.fields.line
        if has_key(a:typeinfo, 'kinddict') &&
         \ has_key(a:typeinfo.kinddict, taginfo.fields.kind) &&
         \ a:typeinfo.getKind(taginfo.fields.kind).stl
            let taginfo.fields.end = line('$')
        else
            let taginfo.fields.end = taginfo.fields.line
        endif
    endif

    if !has_key(taginfo.fields, 'kind')
        call tagbar#debug#log(
            \ "Warning: No 'kind' field found for tag " . a:name . '!')
        return
    endif

    let taginfo.fileinfo = a:fileinfo
    let taginfo.typeinfo = a:typeinfo

    let a:fileinfo.fline[taginfo.fields.line] = taginfo

    if has_key(taginfo.fields, 'typeref')
        let typeref = taginfo.fields.typeref
        let delimit = stridx(typeref, ':')
        let key = strpart(typeref, 0, delimit)
        if key ==# 'typename'
            let taginfo.data_type = substitute(strpart(typeref, delimit + 1), '\t', '', 'g')
        else
            let taginfo.data_type = key
        endif
    endif

    " If this filetype doesn't have any scope information we can stop here
    if !has_key(a:typeinfo, 'scope2kind')
        call a:fileinfo.addTag(taginfo)
        return
    endif

    " Make scope information accessible
    for scope in keys(a:typeinfo.scope2kind)
        if has_key(taginfo.fields, scope)
            let taginfo.scope = scope
            let taginfo.path  = taginfo.fields[scope]
            let taginfo.fullpath = taginfo.path . a:typeinfo.sro . taginfo.name
            break
        endif
    endfor
    let pathlist = split(taginfo.path, '\V' . escape(a:typeinfo.sro, '\'))
    let taginfo.depth = len(pathlist)

    " Needed for folding
    try
        call taginfo.initFoldState(s:known_files)
    catch /^Vim(\a\+):E716:/
        call tagbar#debug#log('Warning: Unknown tag kind: ' . taginfo.fields.kind)
        return
    endtry

    call s:add_tag_recursive({}, taginfo, pathlist)
endfunction

" s:add_tag_recursive() {{{2
function! s:add_tag_recursive(parent, taginfo, pathlist) abort
    if empty(a:pathlist)
        let pseudotags = []
        if empty(a:parent)
            let name_siblings = a:taginfo.fileinfo.getTagsByName(a:taginfo.name)
        else
            let name_siblings = a:parent.getChildrenByName(a:taginfo.name)
        endif

        for tag in name_siblings
            if (tag.fields.kind ==# '?'
              \ || tag.fields.kind ==# a:taginfo.fields.kind)
             \ && (tag.isPseudoTag()
              \ || (!a:taginfo.isSplitTag() && tag.isSplitTag()))
                call add(pseudotags, tag)
            endif
        endfor

        if !empty(pseudotags)
            let pseudotag = pseudotags[0]
            for child in pseudotag.getChildren()
                call a:taginfo.addChild(child)
                let child.parent = a:taginfo
            endfor
        endif

        if empty(a:parent)
            for pseudotag in pseudotags
                call a:taginfo.fileinfo.removeTag(pseudotag)
            endfor
            call a:taginfo.fileinfo.addTag(a:taginfo)
        else
            for pseudotag in pseudotags
                call a:parent.removeChild(pseudotag)
            endfor
            call a:parent.addChild(a:taginfo)
            let a:taginfo.parent = a:parent
        endif

        return
    endif

    " Find the parent tag
    let parentname = a:pathlist[0]
    if empty(a:parent)
        let parents = a:taginfo.fileinfo.getTagsByName(parentname)
    else
        let parents = a:parent.getChildrenByName(parentname)
    endif

    let nearest_parent = {}
    for candidate in parents
        if candidate.fields.kind ==# '?' ||
         \ (has_key(a:taginfo.typeinfo, 'scope2kind') &&
         \  has_key(a:taginfo.typeinfo.scope2kind, a:taginfo.scope) &&
         \  candidate.fields.kind ==# a:taginfo.typeinfo.scope2kind[a:taginfo.scope])
            if empty(nearest_parent)
                let nearest_parent = candidate
            elseif candidate.fields.line > nearest_parent.fields.line &&
                 \ candidate.fields.line <= a:taginfo.fields.line
                let nearest_parent = candidate
            endif
        endif
    endfor

    if !empty(nearest_parent)
        call s:add_tag_recursive(nearest_parent, a:taginfo, a:pathlist[1:])
    else
        " Create a pseudotag as placeholder
        let pseudotag = tagbar#prototypes#pseudotag#new(parentname)
        let pseudotag.fileinfo = a:taginfo.fileinfo
        let pseudotag.typeinfo = a:taginfo.typeinfo

        if empty(a:parent)
            call a:taginfo.fileinfo.addTag(pseudotag)
        else
            call a:parent.addChild(pseudotag)
            let pseudotag.parent = a:parent
        endif

        call s:add_tag_recursive(pseudotag, a:taginfo, a:pathlist[1:])
    endif
endfunction

" Tag lookup {{{1

" s:GetNearbyTag() {{{2
function! s:GetNearbyTag(request, forcecurrent, ...) abort
    if s:nearby_disabled
        return {}
    endif

    let fileinfo = tagbar#state#get_current_file(a:forcecurrent)
    if empty(fileinfo)
        return {}
    endif

    let typeinfo = fileinfo.typeinfo
    let curline = a:0 > 0 ? a:1 : line('.')
    let direction = a:0 > 1 ? a:2 : -1

    let tag = {}

    if direction < 0
        let endline = 1
        let increment = -1
    else
        let endline = line('$')
        let increment = 1
    endif

    for line in range(curline, endline, increment)
        if has_key(fileinfo.fline, line)
            let curtag = fileinfo.fline[line]
            if a:request ==# 'nearest-stl'
                        \ && has_key(typeinfo.kinddict, curtag.fields.kind)
                        \ && typeinfo.getKind(curtag.fields.kind).stl
                let tag = curtag
                break
            elseif a:request ==# 'scoped-stl'
                        \ && has_key(typeinfo.kinddict, curtag.fields.kind)
                        \ && typeinfo.getKind(curtag.fields.kind).stl
                        \ && curtag.fields.line <= curline
                        \ && curline <= curtag.fields.end
                let tag = curtag
                break
            elseif a:request ==# 'nearest'
                let tag = curtag
                break
            endif
        endif
    endfor

    return tag
endfunction

" Auto-update {{{1

" s:AutoUpdate() {{{2
function! s:AutoUpdate(fname, force) abort
    call tagbar#debug#log('AutoUpdate called [' . a:fname . ']')

    let bufnr = bufnr(a:fname)
    let ftype = getbufvar(bufnr, '&filetype')

    if ftype ==# 'tagbar'
        return
    endif

    " Only consider the main filetype in cases like 'python.django'
    let sftype = get(split(ftype, '\.'), 0, '')

    if !s:IsValidFile(a:fname, sftype)
        call tagbar#debug#log('Not a valid file, stopping processing')
        let s:nearby_disabled = 1
        return
    endif

    " Process the file if it's unknown or outdated
    if s:known_files.has(a:fname)
        let curfile = s:known_files.get(a:fname)
        if a:force || empty(curfile) || curfile.ftype != sftype ||
         \ (filereadable(a:fname) && getftime(a:fname) > curfile.mtime)
            call s:ProcessFile(a:fname, sftype)
        endif
    else
        call s:ProcessFile(a:fname, sftype)
    endif

    let fileinfo = s:known_files.get(a:fname)
    if empty(fileinfo)
        return
    endif

    call tagbar#state#set_current_file(fileinfo)
    let s:nearby_disabled = 0
endfunction

" Utility {{{1

" s:IsValidFile() {{{2
function! s:IsValidFile(fname, ftype) abort
    call tagbar#debug#log('Checking if file is valid [' . a:fname . ']')

    if a:fname ==# '' || a:ftype ==# ''
        call tagbar#debug#log('Empty filename or type')
        return 0
    endif

    if !filereadable(a:fname)
        call tagbar#debug#log('File not readable')
        return 0
    endif

    if getbufvar(a:fname, 'tagbar_ignore') == 1
        call tagbar#debug#log('File is marked as ignored')
        return 0
    endif

    if !has_key(s:known_types, a:ftype)
        if exists('g:tagbar_type_' . a:ftype)
            call s:LoadUserTypeDefs(a:ftype)
        else
            call tagbar#debug#log('Unsupported filetype: ' . a:ftype)
            return 0
        endif
    endif

    return 1
endfunction

" s:warning() {{{2
function! s:warning(msg) abort
    echohl WarningMsg
    echomsg a:msg
    echohl None
endfunction

" Public API {{{1

" tagbar#currenttag() {{{2
function! tagbar#currenttag(fmt, default, ...) abort
    let s:statusline_in_use = 1

    if a:0 >= 1
        let longsig   = a:1 =~# 's' || (type(a:1) == type(0) && a:1 != 0)
        let fullpath  = a:1 =~# 'f'
        let prototype = a:1 =~# 'p'
        if a:0 >= 2
            let search_method = a:2
        else
            let search_method = g:tagbar_highlight_method
        endif
    else
        let longsig   = 0
        let fullpath  = 0
        let prototype = 0
        let search_method = g:tagbar_highlight_method
    endif

    if !s:Init(1)
        return a:default
    endif

    let tag = s:GetNearbyTag(search_method, 1)

    if !empty(tag)
        if prototype
            return tag.getPrototype(1)
        else
            return printf(a:fmt, tag.str(longsig, fullpath))
        endif
    else
        return a:default
    endif
endfunction

" tagbar#currenttagtype() {{{2
function! tagbar#currenttagtype(fmt, default) abort
    let s:statusline_in_use = 1
    let tag = s:GetNearbyTag('scoped-stl', 1)

    if empty(tag)
        return a:default
    endif

    let kind = tag.fields.kind
    if kind ==# ''
        return a:default
    endif

    let typeinfo = tag.fileinfo.typeinfo
    if !has_key(typeinfo.kinddict, kind)
        return a:default
    endif
    let plural = typeinfo.kinds[typeinfo.kinddict[kind]].long
    if has_key(s:singular_types, plural)
        let singular = s:singular_types[plural]
    else
        let singular = plural
    endif
    return printf(a:fmt, singular)
endfunction

" tagbar#currentfile() {{{2
function! tagbar#currentfile() abort
    let filename = ''
    if !empty(tagbar#state#get_current_file(1))
        let filename = fnamemodify(tagbar#state#get_current_file(1).fpath, ':t')
    endif
    return filename
endfunction

" tagbar#GetTagNearLine() {{{2
function! tagbar#GetTagNearLine(lnum, ...) abort
    let fmt    = a:0 > 0 ? a:1 : '%s'
    let flags  = a:0 > 1 ? a:2 : ''
    let search_method = a:0 > 2 ? a:3 : g:tagbar_highlight_method

    let longsig   = flags =~# 's'
    let fullpath  = flags =~# 'f'
    let prototype = flags =~# 'p'

    let taginfo = s:GetNearbyTag(search_method, 1, a:lnum)

    if empty(taginfo)
        return ''
    endif

    if prototype
        return taginfo.getPrototype(1)
    else
        return printf(fmt, taginfo.str(longsig, fullpath))
    endif
endfunction

" tagbar#is_paused() {{{2
function! tagbar#is_paused() abort
    return 0
endfunction

" Modeline {{{1
" vim: ts=8 sw=4 sts=4 et foldenable foldmethod=marker foldcolumn=1
